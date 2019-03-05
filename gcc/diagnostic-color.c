/* Output colorization.
   Copyright (C) 2011-2019 Free Software Foundation, Inc.

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
#include "system.h"
#include "diagnostic-color.h"

#ifdef __MINGW32__
#  include <windows.h>
#endif

#include "color-macros.h"

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
struct color_cap
{
  const char *name;
  const char *val;
  unsigned char name_len;
  bool free_val;
};

/* For GCC_COLORS.  */
static struct color_cap color_dict[] =
{
  { "error", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_RED), 5, false },
  { "warning", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_MAGENTA),
	       7, false },
  { "note", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_CYAN), 4, false },
  { "range1", SGR_SEQ (COLOR_FG_GREEN), 6, false },
  { "range2", SGR_SEQ (COLOR_FG_BLUE), 6, false },
  { "locus", SGR_SEQ (COLOR_BOLD), 5, false },
  { "quote", SGR_SEQ (COLOR_BOLD), 5, false },
  { "fixit-insert", SGR_SEQ (COLOR_FG_GREEN), 12, false },
  { "fixit-delete", SGR_SEQ (COLOR_FG_RED), 12, false },
  { "diff-filename", SGR_SEQ (COLOR_BOLD), 13, false },
  { "diff-hunk", SGR_SEQ (COLOR_FG_CYAN), 9, false },
  { "diff-delete", SGR_SEQ (COLOR_FG_RED), 11, false },
  { "diff-insert", SGR_SEQ (COLOR_FG_GREEN), 11, false },
  { "type-diff", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_GREEN), 9, false },
  { NULL, NULL, 0, false }
};

const char *
colorize_start (bool show_color, const char *name, size_t name_len)
{
  struct color_cap const *cap;

  if (!show_color)
    return "";

  for (cap = color_dict; cap->name; cap++)
    if (cap->name_len == name_len
	&& memcmp (cap->name, name, name_len) == 0)
      break;
  if (cap->name == NULL)
    return "";

  return cap->val;
}

const char *
colorize_stop (bool show_color)
{
  return show_color ? SGR_RESET : "";
}

/* Parse GCC_COLORS.  The default would look like:
   GCC_COLORS='error=01;31:warning=01;35:note=01;36:\
   range1=32:range2=34:locus=01:quote=01:\
   fixit-insert=32:fixit-delete=31:'\
   diff-filename=01:diff-hunk=32:diff-delete=31:diff-insert=32:\
   type-diff=01;32'
   No character escaping is needed or supported.  */
static bool
parse_gcc_colors (void)
{
  const char *p, *q, *name, *val;
  char *b;
  size_t name_len = 0, val_len = 0;

  p = getenv ("GCC_COLORS"); /* Plural! */
  if (p == NULL)
    return true;
  if (*p == '\0')
    return false;

  name = q = p;
  val = NULL;
  /* From now on, be well-formed or you're gone.  */
  for (;;)
    if (*q == ':' || *q == '\0')
      {
	struct color_cap *cap;

	if (val)
	  val_len = q - val;
	else
	  name_len = q - name;
	/* Empty name without val (empty cap)
	   won't match and will be ignored.  */
	for (cap = color_dict; cap->name; cap++)
	  if (cap->name_len == name_len
	      && memcmp (cap->name, name, name_len) == 0)
	    break;
	/* If name unknown, go on for forward compatibility.  */
	if (cap->val && val)
	  {
	    if (cap->free_val)
	      free (CONST_CAST (char *, cap->val));
	    b = XNEWVEC (char, val_len + sizeof (SGR_SEQ ("")));
	    memcpy (b, SGR_START, strlen (SGR_START));
	    memcpy (b + strlen (SGR_START), val, val_len);
	    memcpy (b + strlen (SGR_START) + val_len, SGR_END,
		    sizeof (SGR_END));
	    cap->val = (const char *) b;
	    cap->free_val = true;
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

/* Return true if we should use color when in auto mode, false otherwise. */
static bool
should_colorize (void)
{
#ifdef __MINGW32__
  /* For consistency reasons, one should check the handle returned by
     _get_osfhandle(_fileno(stderr)) because the function
     pp_write_text_to_stream() in pretty-print.c calls fputs() on
     that stream.  However, the code below for non-Windows doesn't seem
     to care about it either...  */
  HANDLE h;
  DWORD m;

  h = GetStdHandle (STD_ERROR_HANDLE);
  return (h != INVALID_HANDLE_VALUE) && (h != NULL)
	  && GetConsoleMode (h, &m);
#else
  char const *t = getenv ("TERM");
  return t && strcmp (t, "dumb") != 0 && isatty (STDERR_FILENO);
#endif
}

bool
colorize_init (diagnostic_color_rule_t rule)
{
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
