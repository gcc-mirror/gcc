/* Output colorization.
   Copyright 2011-2013 Free Software Foundation, Inc.

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

/* Select Graphic Rendition (SGR, "\33[...m") strings.  */
/* Also Erase in Line (EL) to Right ("\33[K") by default.  */
/*    Why have EL to Right after SGR?
	 -- The behavior of line-wrapping when at the bottom of the
	    terminal screen and at the end of the current line is often
	    such that a new line is introduced, entirely cleared with
	    the current background color which may be different from the
	    default one (see the boolean back_color_erase terminfo(5)
	    capability), thus scrolling the display by one line.
	    The end of this new line will stay in this background color
	    even after reverting to the default background color with
	    "\33[m', unless it is explicitly cleared again with "\33[K"
	    (which is the behavior the user would instinctively expect
	    from the whole thing).  There may be some unavoidable
	    background-color flicker at the end of this new line because
	    of this (when timing with the monitor's redraw is just right).
	 -- The behavior of HT (tab, "\t") is usually the same as that of
	    Cursor Forward Tabulation (CHT) with a default parameter
	    of 1 ("\33[I"), i.e., it performs pure movement to the next
	    tab stop, without any clearing of either content or screen
	    attributes (including background color); try
	       printf 'asdfqwerzxcv\rASDF\tZXCV\n'
	    in a bash(1) shell to demonstrate this.  This is not what the
	    user would instinctively expect of HT (but is ok for CHT).
	    The instinctive behavior would include clearing the terminal
	    cells that are skipped over by HT with blank cells in the
	    current screen attributes, including background color;
	    the boolean dest_tabs_magic_smso terminfo(5) capability
	    indicates this saner behavior for HT, but only some rare
	    terminals have it (although it also indicates a special
	    glitch with standout mode in the Teleray terminal for which
	    it was initially introduced).  The remedy is to add "\33K"
	    after each SGR sequence, be it START (to fix the behavior
	    of any HT after that before another SGR) or END (to fix the
	    behavior of an HT in default background color that would
	    follow a line-wrapping at the bottom of the screen in another
	    background color, and to complement doing it after START).
	    Piping GCC's output through a pager such as less(1) avoids
	    any HT problems since the pager performs tab expansion.

      Generic disadvantages of this remedy are:
	 -- Some very rare terminals might support SGR but not EL (nobody
	    will use "gcc -fdiagnostics-color" on a terminal that does not
	    support SGR in the first place).
	 -- Having these extra control sequences might somewhat complicate
	    the task of any program trying to parse "gcc -fdiagnostics-color"
	    output in order to extract structuring information from it.
      A specific disadvantage to doing it after SGR START is:
	 -- Even more possible background color flicker (when timing
	    with the monitor's redraw is just right), even when not at the
	    bottom of the screen.
      There are no additional disadvantages specific to doing it after
      SGR END.

      It would be impractical for GCC to become a full-fledged
      terminal program linked against ncurses or the like, so it will
      not detect terminfo(5) capabilities.  */
#define COLOR_SEPARATOR		";"
#define COLOR_NONE		"00"
#define COLOR_BOLD		"01"
#define COLOR_UNDERSCORE	"04"
#define COLOR_BLINK		"05"
#define COLOR_REVERSE		"07"
#define COLOR_FG_BLACK		"30"
#define COLOR_FG_RED		"31"
#define COLOR_FG_GREEN		"32"
#define COLOR_FG_YELLOW		"33"
#define COLOR_FG_BLUE		"34"
#define COLOR_FG_MAGENTA	"35"
#define COLOR_FG_CYAN		"36"
#define COLOR_FG_WHITE		"37"
#define COLOR_BG_BLACK		"40"
#define COLOR_BG_RED		"41"
#define COLOR_BG_GREEN		"42"
#define COLOR_BG_YELLOW		"43"
#define COLOR_BG_BLUE		"44"
#define COLOR_BG_MAGENTA	"45"
#define COLOR_BG_CYAN		"46"
#define COLOR_BG_WHITE		"47"
#define SGR_START		"\33["
#define SGR_END			"m\33[K"
#define SGR_SEQ(str)		SGR_START str SGR_END
#define SGR_RESET		SGR_SEQ("")


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
  { "caret", SGR_SEQ (COLOR_BOLD COLOR_SEPARATOR COLOR_FG_GREEN), 5, false },
  { "locus", SGR_SEQ (COLOR_BOLD), 5, false },
  { "quote", SGR_SEQ (COLOR_BOLD), 5, false },
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
   GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
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

#if defined(_WIN32)
bool
colorize_init (diagnostic_color_rule_t)
{
  return false;
}
#else

/* Return true if we should use color when in auto mode, false otherwise. */
static bool
should_colorize (void)
{
  char const *t = getenv ("TERM");
  return t && strcmp (t, "dumb") != 0 && isatty (STDERR_FILENO);
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
#endif
