/* Implementation of text_art::styled_string.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
#include "pretty-print.h"
#include "intl.h"
#include "diagnostic.h"
#include "selftest.h"
#include "text-art/selftests.h"
#include "text-art/types.h"
#include "color-macros.h"

using namespace text_art;

namespace {

/* Support class for parsing text containing escape codes.
   See e.g. https://en.wikipedia.org/wiki/ANSI_escape_code
   We only support the codes that pretty-print.cc can generate.  */

class escape_code_parser
{
public:
  escape_code_parser (style_manager &sm,
		      std::vector<styled_unichar> &out)
  : m_sm (sm),
    m_out (out),
    m_cur_style_obj (),
    m_cur_style_id (style::id_plain),
    m_state (state::START)
  {
  }

  void on_char (cppchar_t ch)
  {
    switch (m_state)
      {
      default:
	gcc_unreachable ();
      case state::START:
	if (ch == '\033')
	  {
	    /* The start of an escape sequence.  */
	    m_state = state::AFTER_ESC;
	    return;
	  }
	break;
      case state::AFTER_ESC:
	if (ch == '[')
	  {
	    /* ESC [ is a Control Sequence Introducer.  */
	    m_state = state::CS_PARAMETER_BYTES;
	    return;
	  }
	else if (ch == ']')
	  {
	    /* ESC ] is an Operating System Command.  */
	    m_state = state::WITHIN_OSC;
	    return;
	  }
	break;
      case state::CS_PARAMETER_BYTES:
	if (parameter_byte_p (ch))
	  {
	    m_parameter_bytes.push_back ((char)ch);
	    return;
	  }
	else if (intermediate_byte_p (ch))
	  {
	    m_intermediate_bytes.push_back ((char)ch);
	    m_state = state::CS_INTERMEDIATE_BYTES;
	    return;
	  }
	else if (final_byte_p (ch))
	  {
	    on_final_csi_char (ch);
	    return;
	  }
	break;
      case state::CS_INTERMEDIATE_BYTES:
	/* Expect zero or more intermediate bytes.  */
	if (intermediate_byte_p (ch))
	  {
	    m_intermediate_bytes.push_back ((char)ch);
	    return;
	  }
	else if (final_byte_p (ch))
	  {
	    on_final_csi_char (ch);
	    return;
	  }
	break;
      case state::WITHIN_OSC:
	/* Accumulate chars into m_osc_string, until we see an ST or a BEL.  */
	{
	  /* Check for ESC \, the String Terminator (aka "ST").  */
	  if (ch == '\\'
	      && m_osc_string.size () > 0
	      && m_osc_string.back () == '\033')
	    {
	      m_osc_string.pop_back ();
	      on_final_osc_char ();
	      return;
	    }
	  else if (ch == '\a')
	    {
	      // BEL
	      on_final_osc_char ();
	      return;
	    }
	  m_osc_string.push_back (ch);
	  return;
	}
	break;
      }

    /* Test of handling U+FE0F VARIATION SELECTOR-16 to select the emoji
       variation for the previous character.  */
    if (ch == 0xFE0F)
      {
	if (m_out.size () > 0)
	  m_out.back ().set_emoji_variant ();
	return;
      }

    if (cpp_is_combining_char (ch))
      {
	if (m_out.size () > 0)
	  {
	    m_out.back ().add_combining_char (ch);
	    return;
	  }
      }
    /* By default, add the char.  */
    m_out.push_back (styled_unichar (ch, false, m_cur_style_id));
  }

private:
  void on_final_csi_char (cppchar_t ch)
  {
    switch (ch)
      {
      default:
	/* Unrecognized.  */
	break;
      case 'm':
	{
	  /* SGR control sequence.  */
	  if (m_parameter_bytes.empty ())
	    reset_style ();
	  std::vector<int> params (params_from_decimal ());
	  for (auto iter = params.begin (); iter != params.end (); )
	    {
	      const int param = *iter;
	      switch (param)
		{
		default:
		  /* Unrecognized SGR parameter.  */
		  break;
		case 0:
		  reset_style ();
		  break;
		case 1:
		  set_style_bold ();
		  break;
		case 4:
		  set_style_underscore ();
		  break;
		case 5:
		  set_style_blink ();
		  break;

		/* Named foreground colors.  */
		case 30:
		  set_style_fg_color (style::named_color::BLACK);
		  break;
		case 31:
		  set_style_fg_color (style::named_color::RED);
		  break;
		case 32:
		  set_style_fg_color (style::named_color::GREEN);
		  break;
		case 33:
		  set_style_fg_color (style::named_color::YELLOW);
		  break;
		case 34:
		  set_style_fg_color (style::named_color::BLUE);
		  break;
		case 35:
		  set_style_fg_color (style::named_color::MAGENTA);
		  break;
		case 36:
		  set_style_fg_color (style::named_color::CYAN);
		  break;
		case 37:
		  set_style_fg_color (style::named_color::WHITE);
		  break;

		  /* 8-bit and 24-bit color */
		case 38:
		case 48:
		  {
		    const bool fg = (param == 38);
		    iter++;
		    if (iter != params.end ())
		      switch (*(iter++))
			{
			default:
			  break;
			case 5:
			  /* 8-bit color.  */
			  if (iter != params.end ())
			    {
			      const uint8_t col = *(iter++);
			      if (fg)
				set_style_fg_color (style::color (col));
			      else
				set_style_bg_color (style::color (col));
			    }
			  continue;
			case 2:
			  /* 24-bit color.  */
			  if (iter != params.end ())
			    {
			      const uint8_t r = *(iter++);
			      if (iter != params.end ())
				{
				  const uint8_t g = *(iter++);
				  if (iter != params.end ())
				    {
				      const uint8_t b = *(iter++);
				      if (fg)
					set_style_fg_color (style::color (r,
									  g,
									  b));
				      else
					set_style_bg_color (style::color (r,
									  g,
									  b));
				    }
				}
			    }
			  continue;
			}
		    continue;
		  }
		  break;

		/* Named background colors.  */
		case 40:
		  set_style_bg_color (style::named_color::BLACK);
		  break;
		case 41:
		  set_style_bg_color (style::named_color::RED);
		  break;
		case 42:
		  set_style_bg_color (style::named_color::GREEN);
		  break;
		case 43:
		  set_style_bg_color (style::named_color::YELLOW);
		  break;
		case 44:
		  set_style_bg_color (style::named_color::BLUE);
		  break;
		case 45:
		  set_style_bg_color (style::named_color::MAGENTA);
		  break;
		case 46:
		  set_style_bg_color (style::named_color::CYAN);
		  break;
		case 47:
		  set_style_bg_color (style::named_color::WHITE);
		  break;

		/* Named foreground colors, bright.  */
		case 90:
		  set_style_fg_color (style::color (style::named_color::BLACK,
						    true));
		  break;
		case 91:
		  set_style_fg_color (style::color (style::named_color::RED,
						    true));
		  break;
		case 92:
		  set_style_fg_color (style::color (style::named_color::GREEN,
						    true));
		  break;
		case 93:
		  set_style_fg_color (style::color (style::named_color::YELLOW,
						    true));
		  break;
		case 94:
		  set_style_fg_color (style::color (style::named_color::BLUE,
						    true));
		  break;
		case 95:
		  set_style_fg_color (style::color (style::named_color::MAGENTA,
						    true));
		  break;
		case 96:
		  set_style_fg_color (style::color (style::named_color::CYAN,
						    true));
		  break;
		case 97:
		  set_style_fg_color (style::color (style::named_color::WHITE,
						    true));
		  break;

		/* Named foreground colors, bright.  */
		case 100:
		  set_style_bg_color (style::color (style::named_color::BLACK,
						    true));
		  break;
		case 101:
		  set_style_bg_color (style::color (style::named_color::RED,
						    true));
		  break;
		case 102:
		  set_style_bg_color (style::color (style::named_color::GREEN,
						    true));
		  break;
		case 103:
		  set_style_bg_color (style::color (style::named_color::YELLOW,
						    true));
		  break;
		case 104:
		  set_style_bg_color (style::color (style::named_color::BLUE,
						    true));
		  break;
		case 105:
		  set_style_bg_color (style::color (style::named_color::MAGENTA,
						    true));
		  break;
		case 106:
		  set_style_bg_color (style::color (style::named_color::CYAN,
						    true));
		  break;
		case 107:
		  set_style_bg_color (style::color (style::named_color::WHITE,
						    true));
		  break;
		}
	      ++iter;
	    }
	}
	break;
      }
    m_parameter_bytes.clear ();
    m_intermediate_bytes.clear ();
    m_state = state::START;
  }

  void on_final_osc_char ()
  {
    if (!m_osc_string.empty ())
      {
	switch (m_osc_string[0])
	  {
	  default:
	    break;
	  case '8':
	    /* Hyperlink support; see:
	       https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
	       We don't support params, so we expect either:
	       (a) "8;;URL" to begin a url (see pp_begin_url), or
	       (b) "8;;" to end a URL (see pp_end_url).  */
	    if (m_osc_string.size () >= 3
		&& m_osc_string[1] == ';'
		&& m_osc_string[2] == ';')
	      {
		set_style_url (m_osc_string.begin () + 3,
			       m_osc_string.end ());
	      }
	    break;
	  }
      }
    m_osc_string.clear ();
    m_state = state::START;
  }

  std::vector<int> params_from_decimal () const
  {
    std::vector<int> result;

    int curr_int = -1;
    for (auto param_ch : m_parameter_bytes)
      {
	if (param_ch >= '0' && param_ch <= '9')
	  {
	    if (curr_int == -1)
	      curr_int = 0;
	    else
	      curr_int *= 10;
	    curr_int += param_ch - '0';
	  }
	else
	  {
	    if (curr_int != -1)
	      {
		result.push_back (curr_int);
		curr_int = -1;
	      }
	  }
      }
    if (curr_int != -1)
      result.push_back (curr_int);
    return result;
  }

  void refresh_style_id ()
  {
    m_cur_style_id = m_sm.get_or_create_id (m_cur_style_obj);
  }
  void reset_style ()
  {
    m_cur_style_obj = style ();
    refresh_style_id ();
  }
  void set_style_bold ()
  {
    m_cur_style_obj.m_bold = true;
    refresh_style_id ();
  }
  void set_style_underscore ()
  {
    m_cur_style_obj.m_underscore = true;
    refresh_style_id ();
  }
  void set_style_blink ()
  {
    m_cur_style_obj.m_blink = true;
    refresh_style_id ();
  }
  void set_style_fg_color (style::color color)
  {
    m_cur_style_obj.m_fg_color = color;
    refresh_style_id ();
  }
  void set_style_bg_color (style::color color)
  {
    m_cur_style_obj.m_bg_color = color;
    refresh_style_id ();
  }
  void set_style_url (std::vector<cppchar_t>::iterator begin,
		      std::vector<cppchar_t>::iterator end)
  {
    // The empty string means "no URL"
    m_cur_style_obj.m_url = std::vector<cppchar_t> (begin, end);
    refresh_style_id ();
  }

  static bool parameter_byte_p (cppchar_t ch)
  {
    return ch >= 0x30 && ch <= 0x3F;
  }

  static bool intermediate_byte_p (cppchar_t ch)
  {
    return ch >= 0x20 && ch <= 0x2F;
  }

  static bool final_byte_p (cppchar_t ch)
  {
    return ch >= 0x40 && ch <= 0x7E;
  }

  style_manager &m_sm;
  std::vector<styled_unichar> &m_out;

  style m_cur_style_obj;
  style::id_t m_cur_style_id;

  /* Handling of control sequences.  */
  enum class state
  {
   START,

   /* After ESC, expecting '['.  */
   AFTER_ESC,

   /* Expecting zero or more parameter bytes, an
      intermediate byte, or a final byte.  */
   CS_PARAMETER_BYTES,

   /* Expecting zero or more intermediate bytes, or a final byte.  */
   CS_INTERMEDIATE_BYTES,

   /* Within OSC.  */
   WITHIN_OSC

  } m_state;
  std::vector<char> m_parameter_bytes;
  std::vector<char> m_intermediate_bytes;
  std::vector<cppchar_t> m_osc_string;
};

} // anon namespace

/* class text_art::styled_string.  */

/* Construct a styled_string from STR.
   STR is assumed to be UTF-8 encoded and 0-terminated.

   Parse SGR formatting chars from being in-band (within in the sequence
   of chars) to being out-of-band, as style elements.
   We only support parsing the subset of SGR chars that can be emitted
   by pretty-print.cc   */

styled_string::styled_string (style_manager &sm, const char *str)
: m_chars ()
{
  escape_code_parser parser (sm, m_chars);

  /* We don't actually want the display widths here, but
     it's an easy way to decode UTF-8.  */
  cpp_char_column_policy policy (8, cpp_wcwidth);
  cpp_display_width_computation dw (str, strlen (str), policy);
  while (!dw.done ())
    {
      cpp_decoded_char decoded_char;
      dw.process_next_codepoint (&decoded_char);

      if (!decoded_char.m_valid_ch)
	/* Skip bytes that aren't valid UTF-8.  */
	continue;

      /* Decode SGR formatting.  */
      cppchar_t ch = decoded_char.m_ch;
      parser.on_char (ch);
    }
}

styled_string::styled_string (cppchar_t cppchar, bool emoji)
{
  m_chars.push_back (styled_unichar (cppchar, emoji, style::id_plain));
}

styled_string
styled_string::from_fmt_va (style_manager &sm,
			    printer_fn format_decoder,
			    const char *fmt,
			    va_list *args)
{
  text_info text (fmt, args, errno);
  pretty_printer pp;
  pp_show_color (&pp) = true;
  pp.set_url_format (URL_FORMAT_DEFAULT);
  pp_format_decoder (&pp) = format_decoder;
  pp_format (&pp, &text);
  pp_output_formatted_text (&pp);
  styled_string result (sm, pp_formatted_text (&pp));
  return result;
}

styled_string
styled_string::from_fmt (style_manager &sm,
			 printer_fn format_decoder,
			 const char *fmt, ...)
{
  va_list ap;
  va_start (ap, fmt);
  styled_string result = from_fmt_va (sm, format_decoder, fmt, &ap);
  va_end (ap);
  return result;
}

int
styled_string::calc_canvas_width () const
{
  int result = 0;
  for (auto ch : m_chars)
    result += ch.get_canvas_width ();
  return result;
}

void
styled_string::append (const styled_string &suffix)
{
  m_chars.insert<std::vector<styled_unichar>::const_iterator> (m_chars.end (),
							       suffix.begin (),
							       suffix.end ());
}

void
styled_string::set_url (style_manager &sm, const char *url)
{
  for (auto& ch : m_chars)
    {
      const style &existing_style = sm.get_style (ch.get_style_id ());
      style with_url (existing_style);
      with_url.set_style_url (url);
      ch.m_style_id = sm.get_or_create_id (with_url);
    }
}

#if CHECKING_P

namespace selftest {

static void
test_combining_chars ()
{
  /* This really ought to be in libcpp, but we don't have
     selftests there.  */
  ASSERT_FALSE (cpp_is_combining_char (0));
  ASSERT_FALSE (cpp_is_combining_char ('a'));

  /* COMBINING BREVE (U+0306).  */
  ASSERT_TRUE (cpp_is_combining_char (0x0306));

  /* U+5B57 CJK UNIFIED IDEOGRAPH-5B57.  */
  ASSERT_FALSE (cpp_is_combining_char (0x5B57));

  /* U+FE0F VARIATION SELECTOR-16.  */
  ASSERT_FALSE (cpp_is_combining_char (0xFE0F));
}

static void
test_empty ()
{
  style_manager sm;
  styled_string s (sm, "");
  ASSERT_EQ (s.size (), 0);
  ASSERT_EQ (s.calc_canvas_width (), 0);
}

/* Test of a pure ASCII string with no escape codes.  */

static void
test_simple ()
{
  const char *c_str = "hello world!";
  style_manager sm;
  styled_string s (sm, c_str);
  ASSERT_EQ (s.size (), strlen (c_str));
  ASSERT_EQ (s.calc_canvas_width (), (int)strlen (c_str));
  for (size_t i = 0; i < strlen (c_str); i++)
    {
      ASSERT_EQ (s[i].get_code (), (cppchar_t)c_str[i]);
      ASSERT_EQ (s[i].get_style_id (), 0);
    }
}

/* Test of decoding UTF-8.  */

static void
test_pi_from_utf8 ()
{
  /* U+03C0 "GREEK SMALL LETTER PI".  */
  const char * const pi_utf8 = "\xCF\x80";

  style_manager sm;
  styled_string s (sm, pi_utf8);
  ASSERT_EQ (s.size (), 1);
  ASSERT_EQ (s.calc_canvas_width (), 1);
  ASSERT_EQ (s[0].get_code (), 0x03c0);
  ASSERT_EQ (s[0].emoji_variant_p (), false);
  ASSERT_EQ (s[0].double_width_p (), false);
  ASSERT_EQ (s[0].get_style_id (), 0);
}

/* Test of double-width character.  */

static void
test_emoji_from_utf8 ()
{
  /* U+1F642 "SLIGHTLY SMILING FACE".  */
  const char * const emoji_utf8 = "\xF0\x9F\x99\x82";

  style_manager sm;
  styled_string s (sm, emoji_utf8);
  ASSERT_EQ (s.size (), 1);
  ASSERT_EQ (s.calc_canvas_width (), 2);
  ASSERT_EQ (s[0].get_code (), 0x1f642);
  ASSERT_EQ (s[0].double_width_p (), true);
  ASSERT_EQ (s[0].get_style_id (), 0);
}

/* Test of handling U+FE0F VARIATION SELECTOR-16 to select the emoji
   variation for the previous character.  */

static void
test_emoji_variant_from_utf8 ()
{
  const char * const emoji_utf8
    = (/* U+26A0 WARNING SIGN.  */
       "\xE2\x9A\xA0"
       /* U+FE0F VARIATION SELECTOR-16 (emoji variation selector).  */
       "\xEF\xB8\x8F");

  style_manager sm;
  styled_string s (sm, emoji_utf8);
  ASSERT_EQ (s.size (), 1);
  ASSERT_EQ (s.calc_canvas_width (), 1);
  ASSERT_EQ (s[0].get_code (), 0x26a0);
  ASSERT_EQ (s[0].emoji_variant_p (), true);
  ASSERT_EQ (s[0].double_width_p (), false);
  ASSERT_EQ (s[0].get_style_id (), 0);
}

static void
test_emoji_from_codepoint ()
{
  styled_string s ((cppchar_t)0x1f642);
  ASSERT_EQ (s.size (), 1);
  ASSERT_EQ (s.calc_canvas_width (), 2);
  ASSERT_EQ (s[0].get_code (), 0x1f642);
  ASSERT_EQ (s[0].double_width_p (), true);
  ASSERT_EQ (s[0].get_style_id (), 0);
}

static void
test_from_mixed_width_utf8 ()
{
  /* This UTF-8 string literal is of the form
     before mojibake after
   where the Japanese word "mojibake" is written as the following
   four unicode code points:
     U+6587 CJK UNIFIED IDEOGRAPH-6587
     U+5B57 CJK UNIFIED IDEOGRAPH-5B57
     U+5316 CJK UNIFIED IDEOGRAPH-5316
     U+3051 HIRAGANA LETTER KE.
   Each of these is 3 bytes wide when encoded in UTF-8, whereas the
   "before" and "after" are 1 byte per unicode character.  */
  const char * const mixed_width_utf8
    = ("before "

       /* U+6587 CJK UNIFIED IDEOGRAPH-6587
	  UTF-8: 0xE6 0x96 0x87
	  C octal escaped UTF-8: \346\226\207.  */
       "\346\226\207"

       /* U+5B57 CJK UNIFIED IDEOGRAPH-5B57
	  UTF-8: 0xE5 0xAD 0x97
	  C octal escaped UTF-8: \345\255\227.  */
       "\345\255\227"

       /* U+5316 CJK UNIFIED IDEOGRAPH-5316
	  UTF-8: 0xE5 0x8C 0x96
	  C octal escaped UTF-8: \345\214\226.  */
       "\345\214\226"

       /* U+3051 HIRAGANA LETTER KE
	  UTF-8: 0xE3 0x81 0x91
	  C octal escaped UTF-8: \343\201\221.  */
       "\343\201\221"

       " after");

  style_manager sm;
  styled_string s (sm, mixed_width_utf8);
  ASSERT_EQ (s.size (), 6 + 1 + 4 + 1 + 5);
  ASSERT_EQ (sm.get_num_styles (), 1);

  // We expect the Japanese characters to be double width.
  ASSERT_EQ (s.calc_canvas_width (), 6 + 1 + (2 * 4) + 1 + 5);

  ASSERT_EQ (s[0].get_code (), 'b');
  ASSERT_EQ (s[0].double_width_p (), false);
  ASSERT_EQ (s[1].get_code (), 'e');
  ASSERT_EQ (s[2].get_code (), 'f');
  ASSERT_EQ (s[3].get_code (), 'o');
  ASSERT_EQ (s[4].get_code (), 'r');
  ASSERT_EQ (s[5].get_code (), 'e');
  ASSERT_EQ (s[6].get_code (), ' ');
  ASSERT_EQ (s[7].get_code (), 0x6587);
  ASSERT_EQ (s[7].double_width_p (), true);
  ASSERT_EQ (s[8].get_code (), 0x5B57);
  ASSERT_EQ (s[9].get_code (), 0x5316);
  ASSERT_EQ (s[10].get_code (), 0x3051);
  ASSERT_EQ (s[11].get_code (), ' ');
  ASSERT_EQ (s[12].get_code (), 'a');
  ASSERT_EQ (s[13].get_code (), 'f');
  ASSERT_EQ (s[14].get_code (), 't');
  ASSERT_EQ (s[15].get_code (), 'e');
  ASSERT_EQ (s[16].get_code (), 'r');

  ASSERT_EQ (s[0].get_style_id (), 0);
}

static void
assert_style_urleq (const location &loc,
		    const style &s,
		    const char *expected_str)
{
  ASSERT_EQ_AT (loc, s.m_url.size (), strlen (expected_str));
  for (size_t i = 0; i < s.m_url.size (); i++)
    ASSERT_EQ_AT (loc, s.m_url[i], (cppchar_t)expected_str[i]);
}

#define ASSERT_STYLE_URLEQ(STYLE, EXPECTED_STR) \
  assert_style_urleq ((SELFTEST_LOCATION), (STYLE), (EXPECTED_STR))

static void
test_url ()
{
  // URL_FORMAT_ST
  {
    style_manager sm;
    styled_string s
      (sm, "\33]8;;http://example.com\33\\This is a link\33]8;;\33\\");
    const char *expected = "This is a link";
    ASSERT_EQ (s.size (), strlen (expected));
    ASSERT_EQ (s.calc_canvas_width (), (int)strlen (expected));
    ASSERT_EQ (sm.get_num_styles (), 2);
    for (size_t i = 0; i < strlen (expected); i++)
      {
	ASSERT_EQ (s[i].get_code (), (cppchar_t)expected[i]);
	ASSERT_EQ (s[i].get_style_id (), 1);
      }
    ASSERT_STYLE_URLEQ (sm.get_style (1), "http://example.com");
  }

  // URL_FORMAT_BEL
  {
    style_manager sm;
    styled_string s
      (sm, "\33]8;;http://example.com\aThis is a link\33]8;;\a");
    const char *expected = "This is a link";
    ASSERT_EQ (s.size (), strlen (expected));
    ASSERT_EQ (s.calc_canvas_width (), (int)strlen (expected));
    ASSERT_EQ (sm.get_num_styles (), 2);
    for (size_t i = 0; i < strlen (expected); i++)
      {
	ASSERT_EQ (s[i].get_code (), (cppchar_t)expected[i]);
	ASSERT_EQ (s[i].get_style_id (), 1);
      }
    ASSERT_STYLE_URLEQ (sm.get_style (1), "http://example.com");
  }
}

static void
test_from_fmt ()
{
  style_manager sm;
  styled_string s (styled_string::from_fmt (sm, NULL, "%%i: %i", 42));
  ASSERT_EQ (s[0].get_code (), '%');
  ASSERT_EQ (s[1].get_code (), 'i');
  ASSERT_EQ (s[2].get_code (), ':');
  ASSERT_EQ (s[3].get_code (), ' ');
  ASSERT_EQ (s[4].get_code (), '4');
  ASSERT_EQ (s[5].get_code (), '2');
  ASSERT_EQ (s.size (), 6);
  ASSERT_EQ (s.calc_canvas_width (), 6);
}

static void
test_from_fmt_qs ()
{
  auto_fix_quotes fix_quotes;
  open_quote = "\xe2\x80\x98";
  close_quote = "\xe2\x80\x99";

  style_manager sm;
  styled_string s (styled_string::from_fmt (sm, NULL, "%qs", "msg"));
  ASSERT_EQ (sm.get_num_styles (), 2);
  ASSERT_EQ (s[0].get_code (), 0x2018);
  ASSERT_EQ (s[0].get_style_id (), 0);
  ASSERT_EQ (s[1].get_code (), 'm');
  ASSERT_EQ (s[1].get_style_id (), 1);
  ASSERT_EQ (s[2].get_code (), 's');
  ASSERT_EQ (s[2].get_style_id (), 1);
  ASSERT_EQ (s[3].get_code (), 'g');
  ASSERT_EQ (s[3].get_style_id (), 1);
  ASSERT_EQ (s[4].get_code (), 0x2019);
  ASSERT_EQ (s[4].get_style_id (), 0);
  ASSERT_EQ (s.size (), 5);
}

// Test of parsing SGR codes.

static void
test_from_str_with_bold ()
{
  style_manager sm;
  /* This is the result of pp_printf (pp, "%qs", "foo")
     with auto_fix_quotes.  */
  styled_string s (sm, "`\33[01m\33[Kfoo\33[m\33[K'");
  ASSERT_EQ (s[0].get_code (), '`');
  ASSERT_EQ (s[0].get_style_id (), 0);
  ASSERT_EQ (s[1].get_code (), 'f');
  ASSERT_EQ (s[1].get_style_id (), 1);
  ASSERT_EQ (s[2].get_code (), 'o');
  ASSERT_EQ (s[2].get_style_id (), 1);
  ASSERT_EQ (s[3].get_code (), 'o');
  ASSERT_EQ (s[3].get_style_id (), 1);
  ASSERT_EQ (s[4].get_code (), '\'');
  ASSERT_EQ (s[4].get_style_id (), 0);
  ASSERT_EQ (s.size (), 5);
  ASSERT_TRUE (sm.get_style (1).m_bold);
}

static void
test_from_str_with_underscore ()
{
  style_manager sm;
  styled_string s (sm, "\33[04m\33[KA");
  ASSERT_EQ (s[0].get_code (), 'A');
  ASSERT_EQ (s[0].get_style_id (), 1);
  ASSERT_TRUE (sm.get_style (1).m_underscore);
}

static void
test_from_str_with_blink ()
{
  style_manager sm;
  styled_string s (sm, "\33[05m\33[KA");
  ASSERT_EQ (s[0].get_code (), 'A');
  ASSERT_EQ (s[0].get_style_id (), 1);
  ASSERT_TRUE (sm.get_style (1).m_blink);
}

// Test of parsing SGR codes.

static void
test_from_str_with_color ()
{
  style_manager sm;

  styled_string s (sm,
		   ("0"
		    SGR_SEQ (COLOR_FG_RED)
		    "R"
		    SGR_RESET
		    "2"
		    SGR_SEQ (COLOR_FG_GREEN)
		    "G"
		    SGR_RESET
		    "4"));
  ASSERT_EQ (s.size (), 5);
  ASSERT_EQ (sm.get_num_styles (), 3);
  ASSERT_EQ (s[0].get_code (), '0');
  ASSERT_EQ (s[0].get_style_id (), 0);
  ASSERT_EQ (s[1].get_code (), 'R');
  ASSERT_EQ (s[1].get_style_id (), 1);
  ASSERT_EQ (s[2].get_code (), '2');
  ASSERT_EQ (s[2].get_style_id (), 0);
  ASSERT_EQ (s[3].get_code (), 'G');
  ASSERT_EQ (s[3].get_style_id (), 2);
  ASSERT_EQ (s[4].get_code (), '4');
  ASSERT_EQ (s[4].get_style_id (), 0);
  ASSERT_EQ (sm.get_style (1).m_fg_color, style::named_color::RED);
  ASSERT_EQ (sm.get_style (2).m_fg_color, style::named_color::GREEN);
}

static void
test_from_str_with_named_color ()
{
  style_manager sm;
  styled_string s (sm,
		   ("F"
		    SGR_SEQ (COLOR_FG_BLACK) "F"
		    SGR_SEQ (COLOR_FG_RED) "F"
		    SGR_SEQ (COLOR_FG_GREEN) "F"
		    SGR_SEQ (COLOR_FG_YELLOW) "F"
		    SGR_SEQ (COLOR_FG_BLUE) "F"
		    SGR_SEQ (COLOR_FG_MAGENTA) "F"
		    SGR_SEQ (COLOR_FG_CYAN) "F"
		    SGR_SEQ (COLOR_FG_WHITE) "F"
		    SGR_SEQ (COLOR_FG_BRIGHT_BLACK) "F"
		    SGR_SEQ (COLOR_FG_BRIGHT_RED) "F"
		    SGR_SEQ (COLOR_FG_BRIGHT_GREEN) "F"
		    SGR_SEQ (COLOR_FG_BRIGHT_YELLOW) "F"
		    SGR_SEQ (COLOR_FG_BRIGHT_BLUE) "F"
		    SGR_SEQ (COLOR_FG_BRIGHT_MAGENTA) "F"
		    SGR_SEQ (COLOR_FG_BRIGHT_CYAN) "F"
		    SGR_SEQ (COLOR_FG_BRIGHT_WHITE) "F"
		    SGR_SEQ (COLOR_BG_BLACK) "B"
		    SGR_SEQ (COLOR_BG_RED) "B"
		    SGR_SEQ (COLOR_BG_GREEN) "B"
		    SGR_SEQ (COLOR_BG_YELLOW) "B"
		    SGR_SEQ (COLOR_BG_BLUE) "B"
		    SGR_SEQ (COLOR_BG_MAGENTA) "B"
		    SGR_SEQ (COLOR_BG_CYAN) "B"
		    SGR_SEQ (COLOR_BG_WHITE) "B"
		    SGR_SEQ (COLOR_BG_BRIGHT_BLACK) "B"
		    SGR_SEQ (COLOR_BG_BRIGHT_RED) "B"
		    SGR_SEQ (COLOR_BG_BRIGHT_GREEN) "B"
		    SGR_SEQ (COLOR_BG_BRIGHT_YELLOW) "B"
		    SGR_SEQ (COLOR_BG_BRIGHT_BLUE) "B"
		    SGR_SEQ (COLOR_BG_BRIGHT_MAGENTA) "B"
		    SGR_SEQ (COLOR_BG_BRIGHT_CYAN) "B"
		    SGR_SEQ (COLOR_BG_BRIGHT_WHITE) "B"));
  ASSERT_EQ (s.size (), 33);
  for (size_t i = 0; i < s.size (); i++)
    ASSERT_EQ (s[i].get_style_id (), i);
  for (size_t i = 0; i < 17; i++)
    ASSERT_EQ (s[i].get_code (), 'F');
  for (size_t i = 17; i < 33; i++)
    ASSERT_EQ (s[i].get_code (), 'B');
}

static void
test_from_str_with_8_bit_color ()
{
  {
    style_manager sm;
    styled_string s (sm,
		     ("[38;5;232m[KF"));
    ASSERT_EQ (s.size (), 1);
    ASSERT_EQ (s[0].get_code (), 'F');
    ASSERT_EQ (s[0].get_style_id (), 1);
    ASSERT_EQ (sm.get_style (1).m_fg_color, style::color (232));
  }
  {
    style_manager sm;
    styled_string s (sm,
		     ("[48;5;231m[KB"));
    ASSERT_EQ (s.size (), 1);
    ASSERT_EQ (s[0].get_code (), 'B');
    ASSERT_EQ (s[0].get_style_id (), 1);
    ASSERT_EQ (sm.get_style (1).m_bg_color, style::color (231));
  }
}

static void
test_from_str_with_24_bit_color ()
{
  {
    style_manager sm;
    styled_string s (sm,
		     ("[38;2;243;250;242m[KF"));
    ASSERT_EQ (s.size (), 1);
    ASSERT_EQ (s[0].get_code (), 'F');
    ASSERT_EQ (s[0].get_style_id (), 1);
    ASSERT_EQ (sm.get_style (1).m_fg_color, style::color (243, 250, 242));
  }
  {
    style_manager sm;
    styled_string s (sm,
		     ("[48;2;253;247;231m[KB"));
    ASSERT_EQ (s.size (), 1);
    ASSERT_EQ (s[0].get_code (), 'B');
    ASSERT_EQ (s[0].get_style_id (), 1);
    ASSERT_EQ (sm.get_style (1).m_bg_color, style::color (253, 247, 231));
  }
}

static void
test_from_str_combining_characters ()
{
  style_manager sm;
  styled_string s (sm,
		   /* CYRILLIC CAPITAL LETTER U (U+0423).  */
		   "\xD0\xA3"
		   /* COMBINING BREVE (U+0306).  */
		   "\xCC\x86");
  ASSERT_EQ (s.size (), 1);
  ASSERT_EQ (s[0].get_code (), 0x423);
  ASSERT_EQ (s[0].get_combining_chars ().size (), 1);
  ASSERT_EQ (s[0].get_combining_chars ()[0], 0x306);
}

/* Run all selftests in this file.  */

void
text_art_styled_string_cc_tests ()
{
  test_combining_chars ();
  test_empty ();
  test_simple ();
  test_pi_from_utf8 ();
  test_emoji_from_utf8 ();
  test_emoji_variant_from_utf8 ();
  test_emoji_from_codepoint ();
  test_from_mixed_width_utf8 ();
  test_url ();
  test_from_fmt ();
  test_from_fmt_qs ();
  test_from_str_with_bold ();
  test_from_str_with_underscore ();
  test_from_str_with_blink ();
  test_from_str_with_color ();
  test_from_str_with_named_color ();
  test_from_str_with_8_bit_color ();
  test_from_str_with_24_bit_color ();
  test_from_str_combining_characters ();
}

} // namespace selftest


#endif /* #if CHECKING_P */
