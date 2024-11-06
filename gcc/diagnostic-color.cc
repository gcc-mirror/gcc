/* Output colorization.
   Copyright (C) 2011-2024 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "diagnostic-color.h"
#include "diagnostic-url.h"
#include "label-text.h"

#ifdef __MINGW32__
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#endif

#include "color-macros.h"
#include "selftest.h"

/* The context and logic for choosing default --color screen attributes
   (foreground and background colors, etc.) are the following.
      -- There are eight basic colors available, each with its own
	 nominal luminosity to the human eye and foreground/background
	 codes (black [0 %, 30/40], blue [11 %, 34/44], red [30 %, 31/41],
	 magenta [41 %, 35/45], green [59 %, 32/42], cyan [70 %, 36/46],
	 yellow [89 %, 33/43], and white [100 %, 37/47]).
      -- Sometimes, white as a background is actually implemented using
	 a shade of light gray, so that a foreground white can be visible
	 on top of it (but most often not).
      -- Sometimes, black as a foreground is actually implemented using
	 a shade of dark gray, so that it can be visible on top of a
	 background black (but most often not).
      -- Sometimes, more colors are available, as extensions.
      -- Other attributes can be selected/deselected (bold [1/22],
	 underline [4/24], standout/inverse [7/27], blink [5/25], and
	 invisible/hidden [8/28]).  They are sometimes implemented by
	 using colors instead of what their names imply; e.g., bold is
	 often achieved by using brighter colors.  In practice, only bold
	 is really available to us, underline sometimes being mapped by
	 the terminal to some strange color choice, and standout best
	 being left for use by downstream programs such as less(1).
      -- We cannot assume that any of the extensions or special features
	 are available for the purpose of choosing defaults for everyone.
      -- The most prevalent default terminal backgrounds are pure black
	 and pure white, and are not necessarily the same shades of
	 those as if they were selected explicitly with SGR sequences.
	 Some terminals use dark or light pictures as default background,
	 but those are covered over by an explicit selection of background
	 color with an SGR sequence; their users will appreciate their
	 background pictures not be covered like this, if possible.
      -- Some uses of colors attributes is to make some output items
	 more understated (e.g., context lines); this cannot be achieved
	 by changing the background color.
      -- For these reasons, the GCC color defaults should strive not
	 to change the background color from its default, unless it's
	 for a short item that should be highlighted, not understated.
      -- The GCC foreground color defaults (without an explicitly set
	 background) should provide enough contrast to be readable on any
	 terminal with either a black (dark) or white (light) background.
	 This only leaves red, magenta, green, and cyan (and their bold
	 counterparts) and possibly bold blue.  */
/* Default colors. The user can overwrite them using environment
   variable GCC_COLORS.  */
struct color_default
{
  const char *m_name;
  const char *m_val;
};

/* For GCC_COLORS.  */
static const color_default gcc_color_defaults[] =
{
  { "error", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_RED) },
  { "warning", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_MAGENTA) },
  { "note", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_CYAN) },
  { "range1", SGR_SEQ (COLOR_FG_GREEN) },
  { "range2", SGR_SEQ (COLOR_FG_BLUE) },
  { "locus", SGR_SEQ (COLOR_BOLD) },
  { "quote", SGR_SEQ (COLOR_BOLD) },
  { "path", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_CYAN) },
  { "fnname", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_GREEN) },
  { "targs", SGR_SEQ (COLOR_FG_MAGENTA) },
  { "fixit-insert", SGR_SEQ (COLOR_FG_GREEN) },
  { "fixit-delete", SGR_SEQ (COLOR_FG_RED) },
  { "diff-filename", SGR_SEQ (COLOR_BOLD) },
  { "diff-hunk", SGR_SEQ (COLOR_FG_CYAN) },
  { "diff-delete", SGR_SEQ (COLOR_FG_RED) },
  { "diff-insert", SGR_SEQ (COLOR_FG_GREEN) },
  { "type-diff", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_GREEN) },
  { "valid", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_GREEN) },
  { "invalid", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_RED) },
  { "highlight-a", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_GREEN) },
  { "highlight-b", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_BLUE) }
};

class diagnostic_color_dict
{
public:
  diagnostic_color_dict (const color_default *default_values,
			 size_t num_default_values);

  bool parse_envvar_value (const char *const envvar_value);

  const char *get_start_by_name (const char *name, size_t name_len) const;
  const char *get_start_by_name (const char *name) const
  {
    return get_start_by_name (name, strlen (name));
  }

private:
  struct entry
  {
    entry (const color_default &d)
    : m_name (d.m_name),
      m_name_len (strlen (d.m_name)),
      m_val (label_text::borrow (d.m_val))
    {
    }

    const char *m_name;
    size_t m_name_len;
    label_text m_val;
  };

  const entry *get_entry_by_name (const char *name, size_t name_len) const;
  entry *get_entry_by_name (const char *name, size_t name_len);

  std::vector<entry> m_entries;
};

static diagnostic_color_dict *g_color_dict;

const char *
colorize_start (bool show_color, const char *name, size_t name_len)
{
  if (!show_color)
    return "";

  if (!g_color_dict)
    return "";

  return g_color_dict->get_start_by_name (name, name_len);
}

/* Look for an entry named NAME of length NAME_LEN within this
   diagnostic_color_dict, or nullptr if there isn't one.  */

const diagnostic_color_dict::entry *
diagnostic_color_dict::get_entry_by_name (const char *name,
					  size_t name_len) const
{
  for (auto &iter : m_entries)
    if (iter.m_name_len == name_len
	&& memcmp (iter.m_name, name, name_len) == 0)
      return &iter;
  return nullptr;
}

/* Non-const version of the above.  */

diagnostic_color_dict::entry *
diagnostic_color_dict::get_entry_by_name (const char *name,
					  size_t name_len)
{
  for (auto &iter : m_entries)
    if (iter.m_name_len == name_len
	&& memcmp (iter.m_name, name, name_len) == 0)
      return &iter;
  return nullptr;
}

/* Return the SGR codes to start a color entry named NAME of length
   NAME_LEN within this diagnostic_color_dict, or the empty string if
   there isn't one.  */

const char *
diagnostic_color_dict::get_start_by_name (const char *name,
					  size_t name_len) const
{
  if (const entry *e = get_entry_by_name (name, name_len))
    return e->m_val.get ();

  return "";
}

const char *
colorize_stop (bool show_color)
{
  return show_color ? SGR_RESET : "";
}

/* diagnostic_color_dict's ctor.  Initialize it from the given array
   of color_default values.  */

diagnostic_color_dict::
diagnostic_color_dict (const color_default *default_values,
		       size_t num_default_values)
{
  m_entries.reserve (num_default_values);
  for (size_t idx = 0; idx < num_default_values; idx++)
    m_entries.push_back (entry (default_values[idx]));
}

/* Parse a list of color definitions from an environment variable
   value (such as that of GCC_COLORS).
   No character escaping is needed or supported.  */

bool
diagnostic_color_dict::parse_envvar_value (const char *const envvar_value)
{
  /* envvar not set: use the default colors.  */
  if (envvar_value == nullptr)
    return true;

  /* envvar set to empty string: disable colorization.  */
  if (*envvar_value == '\0')
    return false;

  const char *q, *name, *val;
  size_t name_len = 0, val_len = 0;

  name = q = envvar_value;
  val = NULL;
  /* From now on, be well-formed or you're gone.  */
  for (;;)
    if (*q == ':' || *q == '\0')
      {
	if (val)
	  val_len = q - val;
	else
	  name_len = q - name;
	/* Empty name without val (empty cap)
	   won't match and will be ignored.  */
	entry *e = get_entry_by_name (name, name_len);
	/* If name unknown, go on for forward compatibility.  */
	if (e && val)
	  {
	    char *b = XNEWVEC (char, val_len + sizeof (SGR_SEQ ("")));
	    memcpy (b, SGR_START, strlen (SGR_START));
	    memcpy (b + strlen (SGR_START), val, val_len);
	    memcpy (b + strlen (SGR_START) + val_len, SGR_END,
		    sizeof (SGR_END));
	    e->m_val = label_text::take (b);
	  }
	if (*q == '\0')
	  return true;
	name = ++q;
	val = NULL;
      }
    else if (*q == '=')
      {
	if (q == name || val)
	  return true;

	name_len = q - name;
	val = ++q; /* Can be the empty string.  */
      }
    else if (val == NULL)
      q++; /* Accumulate name.  */
    else if (*q == ';' || (*q >= '0' && *q <= '9'))
      q++; /* Accumulate val.  Protect the terminal from being sent
	      garbage.  */
    else
      return true;
}

/* Parse GCC_COLORS.  The default would look like:
   GCC_COLORS='error=01;31:warning=01;35:note=01;36:\
   range1=32:range2=34:locus=01:quote=01:path=01;36:\
   fixit-insert=32:fixit-delete=31:'\
   diff-filename=01:diff-hunk=32:diff-delete=31:diff-insert=32:\
   type-diff=01;32'.  */
static bool
parse_gcc_colors ()
{
  if (!g_color_dict)
    return false;
  return g_color_dict->parse_envvar_value (getenv ("GCC_COLORS")); /* Plural! */
}

/* Return true if we should use color when in auto mode, false otherwise. */
static bool
should_colorize (void)
{
#ifdef __MINGW32__
  /* For consistency reasons, one should check the handle returned by
     _get_osfhandle(_fileno(stderr)) because the function
     pp_write_text_to_stream() in pretty-print.cc calls fputs() on
     that stream.  However, the code below for non-Windows doesn't seem
     to care about it either...  */
  HANDLE handle;
  DWORD mode;
  BOOL isconsole = false;

  handle = GetStdHandle (STD_ERROR_HANDLE);

  if ((handle != INVALID_HANDLE_VALUE) && (handle != NULL))
    isconsole = GetConsoleMode (handle, &mode);

#ifdef ENABLE_VIRTUAL_TERMINAL_PROCESSING
  if (isconsole)
    {
      /* Try to enable processing of VT100 escape sequences */
      mode |= ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
      SetConsoleMode (handle, mode);
    }
#endif

  return isconsole;
#else
  char const *t = getenv ("TERM");
  /* emacs M-x shell sets TERM="dumb".  */
  return t && strcmp (t, "dumb") != 0 && isatty (STDERR_FILENO);
#endif
}

bool
colorize_init (diagnostic_color_rule_t rule)
{
  if (!g_color_dict)
    g_color_dict = new diagnostic_color_dict (gcc_color_defaults,
					      ARRAY_SIZE (gcc_color_defaults));

  switch (rule)
    {
    case DIAGNOSTICS_COLOR_NO:
      return false;
    case DIAGNOSTICS_COLOR_YES:
      return parse_gcc_colors ();
    case DIAGNOSTICS_COLOR_AUTO:
      if (should_colorize ())
	return parse_gcc_colors ();
      else
	return false;
    default:
      gcc_unreachable ();
    }
}

/* Return URL_FORMAT_XXX which tells how we should emit urls
   when in always mode.
   We use GCC_URLS and if that is not defined TERM_URLS.
   If neither is defined the feature is enabled by default.  */

static diagnostic_url_format
parse_env_vars_for_urls ()
{
  const char *p;

  p = getenv ("GCC_URLS"); /* Plural! */
  if (p == NULL)
    p = getenv ("TERM_URLS");

  if (p == NULL)
    return URL_FORMAT_DEFAULT;

  if (*p == '\0')
    return URL_FORMAT_NONE;

  if (!strcmp (p, "no"))
    return URL_FORMAT_NONE;

  if (!strcmp (p, "st"))
    return URL_FORMAT_ST;

  if (!strcmp (p, "bel"))
    return URL_FORMAT_BEL;

  return URL_FORMAT_DEFAULT;
}

/* Return true if we should use urls when in auto mode, false otherwise.  */

static bool
auto_enable_urls ()
{
  const char *term, *colorterm;

  /* First check the terminal is capable of printing color escapes,
     if not URLs won't work either.  */
  if (!should_colorize ())
    return false;

#ifdef __MINGW32__
  HANDLE handle;
  DWORD mode;

  handle = GetStdHandle (STD_ERROR_HANDLE);
  if ((handle == INVALID_HANDLE_VALUE) || (handle == NULL))
    return false;

  /* If ansi escape sequences aren't supported by the console, then URLs will
     print mangled from mingw_ansi_fputs's console API translation. It wouldn't
     be useful even if this weren't the case.  */
  if (GetConsoleMode (handle, &mode)
#ifdef ENABLE_VIRTUAL_TERMINAL_PROCESSING
      && !(mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING)
#endif
      )
    return false;
#endif

  /* xfce4-terminal is known to not implement URLs at this time.
     Recently new installations (0.8) will safely ignore the URL escape
     sequences, but a large number of legacy installations (0.6.3) print
     garbage when URLs are printed.  Therefore we lose nothing by
     disabling this feature for that specific terminal type.  */
  colorterm = getenv ("COLORTERM");
  if (colorterm && !strcmp (colorterm, "xfce4-terminal"))
    return false;

  /* Old versions of gnome-terminal where URL escapes cause screen
     corruptions set COLORTERM="gnome-terminal", recent versions
     with working URL support set this to "truecolor".  */
  if (colorterm && !strcmp (colorterm, "gnome-terminal"))
    return false;

  /* Since the following checks are less specific than the ones
     above, let GCC_URLS and TERM_URLS override the decision.  */
  if (getenv ("GCC_URLS") || getenv ("TERM_URLS"))
    return true;

  /* In an ssh session the COLORTERM is not there, but TERM=xterm
     can be used as an indication of a incompatible terminal while
     TERM=xterm-256color appears to be a working terminal.  */
  term = getenv ("TERM");
  if (!colorterm && term && !strcmp (term, "xterm"))
    return false;

  /* When logging in a linux over serial line, we see TERM=linux
     and no COLORTERM, it is unlikely that the URL escapes will
     work in that environmen either.  */
  if (!colorterm && term && !strcmp (term, "linux"))
    return false;

  return true;
}

/* Determine if URLs should be enabled, based on RULE,
   and, if so, which format to use.
   This reuses the logic for colorization.  */

diagnostic_url_format
determine_url_format (diagnostic_url_rule_t rule)
{
  switch (rule)
    {
    case DIAGNOSTICS_URL_NO:
      return URL_FORMAT_NONE;
    case DIAGNOSTICS_URL_YES:
      return parse_env_vars_for_urls ();
    case DIAGNOSTICS_URL_AUTO:
      if (auto_enable_urls ())
	return parse_env_vars_for_urls ();
      else
	return URL_FORMAT_NONE;
    default:
      gcc_unreachable ();
    }
}

#if CHECKING_P

namespace selftest {

/* Test of an empty diagnostic_color_dict.  */

static void
test_empty_color_dict ()
{
  diagnostic_color_dict d (nullptr, 0);
  ASSERT_STREQ (d.get_start_by_name ("warning"), "");
  ASSERT_STREQ (d.get_start_by_name ("should-not-be-found"), "");
}

/* Test of a diagnostic_color_dict with GCC's defaults.  */

static void
test_default_color_dict ()
{
  diagnostic_color_dict d (gcc_color_defaults,
			   ARRAY_SIZE (gcc_color_defaults));
  ASSERT_STREQ (d.get_start_by_name ("warning"),
		SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_MAGENTA));
  ASSERT_STREQ (d.get_start_by_name ("should-not-be-found"), "");
}

/* Test of a diagnostic_color_dict with GCC's defaults plus overrides from
   an environment variable.  */

static void
test_color_dict_envvar_parsing ()
{
  diagnostic_color_dict d (gcc_color_defaults,
			   ARRAY_SIZE (gcc_color_defaults));

  d.parse_envvar_value ("error=01;37:warning=01;42:unknown-value=01;36");

  ASSERT_STREQ (d.get_start_by_name ("error"),
		SGR_SEQ ("01;37"));
  ASSERT_STREQ (d.get_start_by_name ("warning"),
		SGR_SEQ ("01;42"));
  ASSERT_STREQ (d.get_start_by_name ("unknown-value"), "");
  ASSERT_STREQ (d.get_start_by_name ("should-not-be-found"), "");
}


/* Run all of the selftests within this file.  */

void
diagnostic_color_cc_tests ()
{
  test_empty_color_dict ();
  test_default_color_dict ();
  test_color_dict_envvar_parsing ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
