/* Classes for styling text cells (color, URLs).
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
#define INCLUDE_ALGORITHM
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
#include "pretty-print.h"
#include "intl.h"
#include "selftest.h"
#include "text-art/selftests.h"
#include "text-art/types.h"
#include "color-macros.h"
#include "diagnostic-color.h"

using namespace text_art;

/* class text_art::style.  */

style &
style::set_style_url (const char *url)
{
  m_url.clear ();
  while (*url)
    m_url.push_back (*(url++));
  return *this;
}

/* class text_art::style::color.  */

bool
style::color::operator== (const style::color &other) const
{
  if (m_kind != other.m_kind)
    return false;
  switch (m_kind)
    {
    default:
      gcc_unreachable ();
    case kind::NAMED:
      return (u.m_named.m_name == other.u.m_named.m_name
	      && u.m_named.m_bright == other.u.m_named.m_bright);
    case kind::BITS_8:
      return u.m_8bit == other.u.m_8bit;
    case kind::BITS_24:
      return (u.m_24bit.r == other.u.m_24bit.r
	      && u.m_24bit.g == other.u.m_24bit.g
	      && u.m_24bit.b == other.u.m_24bit.b);
    }
}

static void
ensure_separator (pretty_printer *pp, bool &need_separator)
{
  if (need_separator)
    pp_string (pp, COLOR_SEPARATOR);
  need_separator = true;
}

void
style::color::print_sgr (pretty_printer *pp,
			 bool fg,
			 bool &need_separator) const
{
  switch (m_kind)
    {
    default:
      gcc_unreachable ();
    case kind::NAMED:
      {
	static const char * const fg_normal[] = {"", // reset, for DEFAULT
						 COLOR_FG_BLACK,
						 COLOR_FG_RED,
						 COLOR_FG_GREEN,
						 COLOR_FG_YELLOW,
						 COLOR_FG_BLUE,
						 COLOR_FG_MAGENTA,
						 COLOR_FG_CYAN,
						 COLOR_FG_WHITE};
	static const char * const fg_bright[] = {"", // reset, for DEFAULT
						 COLOR_FG_BRIGHT_BLACK,
						 COLOR_FG_BRIGHT_RED,
						 COLOR_FG_BRIGHT_GREEN,
						 COLOR_FG_BRIGHT_YELLOW,
						 COLOR_FG_BRIGHT_BLUE,
						 COLOR_FG_BRIGHT_MAGENTA,
						 COLOR_FG_BRIGHT_CYAN,
						 COLOR_FG_BRIGHT_WHITE};
	static const char * const bg_normal[] = {"", // reset, for DEFAULT
						 COLOR_BG_BLACK,
						 COLOR_BG_RED,
						 COLOR_BG_GREEN,
						 COLOR_BG_YELLOW,
						 COLOR_BG_BLUE,
						 COLOR_BG_MAGENTA,
						 COLOR_BG_CYAN,
						 COLOR_BG_WHITE};
	static const char * const bg_bright[] = {"", // reset, for DEFAULT
						 COLOR_BG_BRIGHT_BLACK,
						 COLOR_BG_BRIGHT_RED,
						 COLOR_BG_BRIGHT_GREEN,
						 COLOR_BG_BRIGHT_YELLOW,
						 COLOR_BG_BRIGHT_BLUE,
						 COLOR_BG_BRIGHT_MAGENTA,
						 COLOR_BG_BRIGHT_CYAN,
						 COLOR_BG_BRIGHT_WHITE};
	STATIC_ASSERT (ARRAY_SIZE (fg_normal) == ARRAY_SIZE (fg_bright));
	STATIC_ASSERT (ARRAY_SIZE (fg_normal) == ARRAY_SIZE (bg_normal));
	STATIC_ASSERT (ARRAY_SIZE (fg_normal) == ARRAY_SIZE (bg_bright));
	gcc_assert ((size_t)u.m_named.m_name < ARRAY_SIZE (fg_normal));
	const char *const *arr;
	if (fg)
	  arr = u.m_named.m_bright ? fg_bright : fg_normal;
	else
	  arr = u.m_named.m_bright ? bg_bright : bg_normal;
	const char *str = arr[(size_t)u.m_named.m_name];
	if (strlen (str) > 0)
	  {
	    ensure_separator (pp, need_separator);
	    pp_string (pp, str);
	  }
      }
      break;
    case kind::BITS_8:
      {
	ensure_separator (pp, need_separator);
	if (fg)
	  pp_string (pp, "38");
	else
	  pp_string (pp, "48");
	pp_printf (pp, ";5;%i", (int)u.m_8bit);
      }
      break;
    case kind::BITS_24:
      {
	ensure_separator (pp, need_separator);
	if (fg)
	  pp_string (pp, "38");
	else
	  pp_string (pp, "48");
	pp_printf (pp, ";2;%i;%i;%i",
		   (int)u.m_24bit.r,
		   (int)u.m_24bit.g,
		   (int)u.m_24bit.b);
      }
      break;
    }
}

/* class text_art::style.  */

/* See https://www.ecma-international.org/wp-content/uploads/ECMA-48_5th_edition_june_1991.pdf
   GRCM - GRAPHIC RENDITION COMBINATION MODE can be "REPLACING" or
   "CUMULATIVE", which affects whether we need to respecify all attributes
   at each SGR, or can accumulate them.  Looks like we can't rely on the value
   of this, so we have to emit a single SGR for all changes, with a "0" reset
   at the front, forcing it to be effectively replacing.  */

void
style::print_changes (pretty_printer *pp,
		      const style &old_style,
		      const style &new_style)
{
  if (pp_show_color (pp))
    {
      bool needs_sgr = ((old_style.m_bold != new_style.m_bold)
			|| (old_style.m_underscore != new_style.m_underscore)
			|| (old_style.m_blink != new_style.m_blink)
			|| (old_style.m_fg_color != new_style.m_fg_color)
			|| (old_style.m_bg_color != new_style.m_bg_color));
      if (needs_sgr)
	{
	  bool emit_reset = (old_style.m_bold
			     || new_style.m_bold
			     || old_style.m_underscore
			     || new_style.m_underscore
			     || old_style.m_blink
			     || new_style.m_blink);
	  bool need_separator = false;

	  pp_string (pp, SGR_START);
	  if (emit_reset)
	    {
	      pp_string (pp, COLOR_NONE);
	      need_separator = true;
	    }
	  if (new_style.m_bold)
	    {
	      gcc_assert (emit_reset);
	      ensure_separator (pp, need_separator);
	      pp_string (pp, COLOR_BOLD);
	    }
	  if (new_style.m_underscore)
	    {
	      gcc_assert (emit_reset);
	      ensure_separator (pp, need_separator);
	      pp_string (pp, COLOR_UNDERSCORE);
	    }
	  if (new_style.m_blink)
	    {
	      gcc_assert (emit_reset);
	      ensure_separator (pp, need_separator);
	      pp_string (pp, COLOR_BLINK);
	    }
	  new_style.m_fg_color.print_sgr (pp, true, need_separator);
	  new_style.m_bg_color.print_sgr (pp, false, need_separator);
	  pp_string (pp, SGR_END);
	}
    }

  if (old_style.m_url != new_style.m_url)
    {
      if (!old_style.m_url.empty ())
	pp_end_url (pp);
      if (pp->url_format != URL_FORMAT_NONE
	  && !new_style.m_url.empty ())
	{
	  /* Adapted from pp_begin_url, but encoding the
	     chars to UTF-8 on the fly, rather than converting
	     to a buffer.  */
	  pp_string (pp, "\33]8;;");
	  for (auto ch : new_style.m_url)
	    pp_unicode_character (pp, ch);
	  switch (pp->url_format)
	    {
	    default:
	    case URL_FORMAT_NONE:
	      gcc_unreachable ();
	    case URL_FORMAT_ST:
	      pp_string (pp, "\33\\");
	      break;
	    case URL_FORMAT_BEL:
	      pp_string (pp, "\a");
	      break;
	    }
	}
    }
}

/* Look up the current SGR codes for a color capability NAME
   (from GCC_COLORS or the defaults), and convert them to
   a text_art::style.  */

style
text_art::get_style_from_color_cap_name (const char *name)
{
  const char *sgr_codes = colorize_start (true, name);
  gcc_assert (sgr_codes);

  /* Parse the sgr codes.  We expect the resulting styled_string to be
     empty; we're interested in the final style created during parsing.  */
  style_manager sm;
  styled_string styled_str (sm, sgr_codes);
  return sm.get_style (sm.get_num_styles () - 1);
}

/* class text_art::style_manager.  */

style_manager::style_manager ()
{
  // index 0 will be the default style
  m_styles.push_back (style ());
}

style::id_t
style_manager::get_or_create_id (const style &s)
{
  // For now, linear search
  std::vector<style>::iterator existing
    (std::find (m_styles.begin (), m_styles.end (), s));

  /* If found, return index of slot.  */
  if (existing != m_styles.end ())
    return std::distance (m_styles.begin (), existing);

  /* Not found.  */

  /* styled_str uses 7 bits for style information, so we can only support
     up to 128 different style combinations.
     Gracefully fail by turning off styling when this limit is reached.  */
  if (m_styles.size () >= 127)
    return 0;

  m_styles.push_back (s);
  return m_styles.size () - 1;
}

void
style_manager::print_any_style_changes (pretty_printer *pp,
					style::id_t old_id,
					style::id_t new_id) const
{
  gcc_assert (pp);
  if (old_id == new_id)
    return;

  const style &old_style = m_styles[old_id];
  const style &new_style = m_styles[new_id];
  gcc_assert (!(old_style == new_style));
  style::print_changes (pp, old_style, new_style);
}

#if CHECKING_P

namespace selftest {

void
assert_style_change_streq (const location &loc,
			   const style &old_style,
			   const style &new_style,
			   const char *expected_str)
{
  pretty_printer pp;
  pp_show_color (&pp) = true;
  style::print_changes (&pp, old_style, new_style);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected_str);
}

#define ASSERT_STYLE_CHANGE_STREQ(OLD_STYLE, NEW_STYLE, EXPECTED_STR) \
  SELFTEST_BEGIN_STMT						      \
    assert_style_change_streq ((SELFTEST_LOCATION),		      \
			       (OLD_STYLE),			      \
			       (NEW_STYLE),			      \
			       (EXPECTED_STR));			      \
  SELFTEST_END_STMT

static void
test_bold ()
{
  style_manager sm;
  ASSERT_EQ (sm.get_num_styles (), 1);

  style plain;
  ASSERT_EQ (sm.get_or_create_id (plain), 0);
  ASSERT_EQ (sm.get_num_styles (), 1);

  style bold;
  bold.m_bold = true;

  ASSERT_EQ (sm.get_or_create_id (bold), 1);
  ASSERT_EQ (sm.get_num_styles (), 2);
  ASSERT_EQ (sm.get_or_create_id (bold), 1);
  ASSERT_EQ (sm.get_num_styles (), 2);

  ASSERT_STYLE_CHANGE_STREQ (plain, bold, "\33[00;01m\33[K");
  ASSERT_STYLE_CHANGE_STREQ (bold, plain, "\33[00m\33[K");
}

static void
test_underscore ()
{
  style_manager sm;
  ASSERT_EQ (sm.get_num_styles (), 1);

  style plain;
  ASSERT_EQ (sm.get_or_create_id (plain), 0);
  ASSERT_EQ (sm.get_num_styles (), 1);

  style underscore;
  underscore.m_underscore = true;

  ASSERT_EQ (sm.get_or_create_id (underscore), 1);
  ASSERT_EQ (sm.get_num_styles (), 2);
  ASSERT_EQ (sm.get_or_create_id (underscore), 1);
  ASSERT_EQ (sm.get_num_styles (), 2);

  ASSERT_STYLE_CHANGE_STREQ (plain, underscore, "\33[00;04m\33[K");
  ASSERT_STYLE_CHANGE_STREQ (underscore, plain, "\33[00m\33[K");
}

static void
test_blink ()
{
  style_manager sm;
  ASSERT_EQ (sm.get_num_styles (), 1);

  style plain;
  ASSERT_EQ (sm.get_or_create_id (plain), 0);
  ASSERT_EQ (sm.get_num_styles (), 1);

  style blink;
  blink.m_blink = true;

  ASSERT_EQ (sm.get_or_create_id (blink), 1);
  ASSERT_EQ (sm.get_num_styles (), 2);
  ASSERT_EQ (sm.get_or_create_id (blink), 1);
  ASSERT_EQ (sm.get_num_styles (), 2);

  ASSERT_STYLE_CHANGE_STREQ (plain, blink, "\33[00;05m\33[K");
  ASSERT_STYLE_CHANGE_STREQ (blink, plain, "\33[00m\33[K");
}

#define ASSERT_NAMED_COL_STREQ(NAMED_COLOR, FG, BRIGHT, EXPECTED_STR) \
  SELFTEST_BEGIN_STMT						      \
  {								      \
    style plain;						      \
    style s;							      \
    if (FG)							      \
      s.m_fg_color = style::color ((NAMED_COLOR), (BRIGHT));	      \
    else							      \
      s.m_bg_color = style::color ((NAMED_COLOR), (BRIGHT));	      \
    assert_style_change_streq ((SELFTEST_LOCATION),		      \
			       plain,				      \
			       s,				      \
			       (EXPECTED_STR));			      \
  }								      \
  SELFTEST_END_STMT

static void
test_named_colors ()
{
  /* Foreground colors.  */
  {
    const bool fg = true;
    {
      const bool bright = false;
      ASSERT_NAMED_COL_STREQ (style::named_color::DEFAULT, fg, bright, "");
      ASSERT_NAMED_COL_STREQ (style::named_color::BLACK, fg, bright,
			      "[30m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::RED, fg, bright,
			      "[31m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::GREEN, fg, bright,
			      "[32m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::YELLOW, fg, bright,
			      "[33m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::BLUE, fg, bright,
			      "[34m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::MAGENTA, fg, bright,
			      "[35m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::CYAN, fg, bright,
			      "[36m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::WHITE, fg, bright,
			      "[37m[K");
    }
    {
      const bool bright = true;
      ASSERT_NAMED_COL_STREQ (style::named_color::DEFAULT, fg, bright,
			      "[m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::BLACK, fg, bright,
			      "[90m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::RED, fg, bright,
			      "[91m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::GREEN, fg, bright,
			      "[92m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::YELLOW, fg, bright,
			      "[93m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::BLUE, fg, bright,
			      "[94m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::MAGENTA, fg, bright,
			      "[95m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::CYAN, fg, bright,
			      "[96m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::WHITE, fg, bright,
			      "[97m[K");
    }
  }

  /* Background colors.  */
  {
    const bool fg = false;
    {
      const bool bright = false;
      ASSERT_NAMED_COL_STREQ (style::named_color::DEFAULT, fg, bright, "");
      ASSERT_NAMED_COL_STREQ (style::named_color::BLACK, fg, bright,
			      "[40m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::RED, fg, bright,
			      "[41m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::GREEN, fg, bright,
			      "[42m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::YELLOW, fg, bright,
			      "[43m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::BLUE, fg, bright,
			      "[44m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::MAGENTA, fg, bright,
			      "[45m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::CYAN, fg, bright,
			      "[46m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::WHITE, fg, bright,
			      "[47m[K");
    }
    {
      const bool bright = true;
      ASSERT_NAMED_COL_STREQ (style::named_color::DEFAULT, fg, bright,
			      "[m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::BLACK, fg, bright,
			      "[100m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::RED, fg, bright,
			      "[101m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::GREEN, fg, bright,
			      "[102m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::YELLOW, fg, bright,
			      "[103m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::BLUE, fg, bright,
			      "[104m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::MAGENTA, fg, bright,
			      "[105m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::CYAN, fg, bright,
			      "[106m[K");
      ASSERT_NAMED_COL_STREQ (style::named_color::WHITE, fg, bright,
			      "[107m[K");
    }
  }
}

#define ASSERT_8_BIT_COL_STREQ(COL_VAL, FG, EXPECTED_STR) \
  SELFTEST_BEGIN_STMT						      \
  {								      \
    style plain;						      \
    style s;							      \
    if (FG)							      \
      s.m_fg_color = style::color (COL_VAL);			      \
    else							      \
      s.m_bg_color = style::color (COL_VAL);			      \
    assert_style_change_streq ((SELFTEST_LOCATION),		      \
			       plain,				      \
			       s,				      \
			       (EXPECTED_STR));			      \
  }								      \
  SELFTEST_END_STMT

static void
test_8_bit_colors ()
{
  /* Foreground colors.  */
  {
    const bool fg = true;
    /* 0-15: standard and high-intensity standard colors.  */
    ASSERT_8_BIT_COL_STREQ (0, fg, "[38;5;0m[K");
    ASSERT_8_BIT_COL_STREQ (15, fg, "[38;5;15m[K");
    /* 16-231: 6x6x6 color cube.  */
    ASSERT_8_BIT_COL_STREQ (16, fg, "[38;5;16m[K");
    ASSERT_8_BIT_COL_STREQ (231, fg, "[38;5;231m[K");
    /* 232-255: grayscale.  */
    ASSERT_8_BIT_COL_STREQ (232, fg, "[38;5;232m[K");
    ASSERT_8_BIT_COL_STREQ (255, fg, "[38;5;255m[K");
  }
  /* Background colors.  */
  {
    const bool fg = false;
    /* 0-15: standard and high-intensity standard colors.  */
    ASSERT_8_BIT_COL_STREQ (0, fg, "[48;5;0m[K");
    ASSERT_8_BIT_COL_STREQ (15, fg, "[48;5;15m[K");
    /* 16-231: 6x6x6 color cube.  */
    ASSERT_8_BIT_COL_STREQ (16, fg, "[48;5;16m[K");
    ASSERT_8_BIT_COL_STREQ (231, fg, "[48;5;231m[K");
    /* 232-255: grayscale.  */
    ASSERT_8_BIT_COL_STREQ (232, fg, "[48;5;232m[K");
    ASSERT_8_BIT_COL_STREQ (255, fg, "[48;5;255m[K");
  }
}

#define ASSERT_24_BIT_COL_STREQ(R, G, B, FG, EXPECTED_STR)	      \
  SELFTEST_BEGIN_STMT						      \
  {								      \
    style plain;						      \
    style s;							      \
    if (FG)							      \
      s.m_fg_color = style::color ((R), (G), (B));		      \
    else							      \
      s.m_bg_color = style::color ((R), (G), (B));		      \
    assert_style_change_streq ((SELFTEST_LOCATION),		      \
			       plain,				      \
			       s,				      \
			       (EXPECTED_STR));			      \
  }								      \
  SELFTEST_END_STMT

static void
test_24_bit_colors ()
{
  /* Foreground colors.  */
  {
    const bool fg = true;
    // #F3FAF2:
    ASSERT_24_BIT_COL_STREQ (0xf3, 0xfa, 0xf2, fg,
			     "[38;2;243;250;242m[K");
  }
  /* Background colors.  */
  {
    const bool fg = false;
    // #FDF7E7
    ASSERT_24_BIT_COL_STREQ (0xfd, 0xf7, 0xe7, fg,
			     "[48;2;253;247;231m[K");
  }
}

static void
test_style_combinations ()
{
  style_manager sm;
  ASSERT_EQ (sm.get_num_styles (), 1);

  style plain;
  ASSERT_EQ (sm.get_or_create_id (plain), 0);
  ASSERT_EQ (sm.get_num_styles (), 1);

  style bold;
  bold.m_bold = true;

  ASSERT_EQ (sm.get_or_create_id (bold), 1);
  ASSERT_EQ (sm.get_num_styles (), 2);
  ASSERT_EQ (sm.get_or_create_id (bold), 1);
  ASSERT_EQ (sm.get_num_styles (), 2);

  style magenta_on_blue;
  magenta_on_blue.m_fg_color = style::named_color::MAGENTA;
  magenta_on_blue.m_bg_color = style::named_color::BLUE;
  ASSERT_EQ (sm.get_or_create_id (magenta_on_blue), 2);
  ASSERT_EQ (sm.get_num_styles (), 3);
  ASSERT_EQ (sm.get_or_create_id (magenta_on_blue), 2);
  ASSERT_EQ (sm.get_num_styles (), 3);
}

/* Run all selftests in this file.  */

void
text_art_style_cc_tests ()
{
  test_bold ();
  test_underscore ();
  test_blink ();
  test_named_colors ();
  test_8_bit_colors ();
  test_24_bit_colors ();
  test_style_combinations ();
}

} // namespace selftest


#endif /* #if CHECKING_P */
