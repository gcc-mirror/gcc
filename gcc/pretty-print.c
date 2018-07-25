/* Various declarations for language-independent pretty-print subroutines.
   Copyright (C) 2003-2018 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

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
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include "pretty-print.h"
#include "diagnostic-color.h"
#include "selftest.h"

#if HAVE_ICONV
#include <iconv.h>
#endif

#ifdef __MINGW32__

/* Replacement for fputs() that handles ANSI escape codes on Windows NT.
   Contributed by: Liu Hao (lh_mouse at 126 dot com)

   XXX: This file is compiled into libcommon.a that will be self-contained.
	It looks like that these functions can be put nowhere else.  */

#include <io.h>
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>

/* Write all bytes in [s,s+n) into the specified stream.
   Errors are ignored.  */
static void
write_all (HANDLE h, const char *s, size_t n)
{
  size_t rem = n;
  DWORD step;

  while (rem != 0)
    {
      if (rem <= UINT_MAX)
	step = rem;
      else
	step = UINT_MAX;
      if (!WriteFile (h, s + n - rem, step, &step, NULL))
	break;
      rem -= step;
    }
}

/* Find the beginning of an escape sequence.
   There are two cases:
   1. If the sequence begins with an ESC character (0x1B) and a second
      character X in [0x40,0x5F], returns X and stores a pointer to
      the third character into *head.
   2. If the sequence begins with a character X in [0x80,0x9F], returns
      (X-0x40) and stores a pointer to the second character into *head.
   Stores the number of ESC character(s) in *prefix_len.
   Returns 0 if no such sequence can be found.  */
static int
find_esc_head (int *prefix_len, const char **head, const char *str)
{
  int c;
  const char *r = str;
  int escaped = 0;

  for (;;)
    {
      c = (unsigned char) *r;
      if (c == 0)
	{
	  /* Not found.  */
	  return 0;
	}
      if (escaped && 0x40 <= c && c <= 0x5F)
	{
	  /* Found (case 1).  */
	  *prefix_len = 2;
	  *head = r + 1;
	  return c;
	}
      if (0x80 <= c && c <= 0x9F)
	{
	  /* Found (case 2).  */
	  *prefix_len = 1;
	  *head = r + 1;
	  return c - 0x40;
	}
      ++r;
      escaped = c == 0x1B;
    }
}

/* Find the terminator of an escape sequence.
   str should be the value stored in *head by a previous successful
   call to find_esc_head().
   Returns 0 if no such sequence can be found.  */
static int
find_esc_terminator (const char **term, const char *str)
{
  int c;
  const char *r = str;

  for (;;)
    {
      c = (unsigned char) *r;
      if (c == 0)
	{
	  /* Not found.  */
	  return 0;
	}
      if (0x40 <= c && c <= 0x7E)
	{
	  /* Found.  */
	  *term = r;
	  return c;
	}
      ++r;
    }
}

/* Handle a sequence of codes.  Sequences that are invalid, reserved,
   unrecognized or unimplemented are ignored silently.
   There isn't much we can do because of lameness of Windows consoles.  */
static void
eat_esc_sequence (HANDLE h, int esc_code,
		  const char *esc_head, const char *esc_term)
{
  /* Numbers in an escape sequence cannot be negative, because
     a minus sign in the middle of it would have terminated it.  */
  long n1, n2;
  char *eptr, *delim;
  CONSOLE_SCREEN_BUFFER_INFO sb;
  COORD cr;
  /* ED and EL parameters.  */
  DWORD cnt, step;
  long rows;
  /* SGR parameters.  */
  WORD attrib_add, attrib_rm;
  const char *param;

  switch (MAKEWORD (esc_code, *esc_term))
    {
    /* ESC [ n1 'A'
	 Move the cursor up by n1 characters.  */
    case MAKEWORD ('[', 'A'):
      if (esc_head == esc_term)
	n1 = 1;
      else
	{
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  cr = sb.dwCursorPosition;
	  /* Stop at the topmost boundary.  */
	  if (cr.Y > n1)
	    cr.Y -= n1;
	  else
	    cr.Y = 0;
	  SetConsoleCursorPosition (h, cr);
	}
      break;

    /* ESC [ n1 'B'
	 Move the cursor down by n1 characters.  */
    case MAKEWORD ('[', 'B'):
      if (esc_head == esc_term)
	n1 = 1;
      else
	{
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  cr = sb.dwCursorPosition;
	  /* Stop at the bottommost boundary.  */
	  if (sb.dwSize.Y - cr.Y > n1)
	    cr.Y += n1;
	  else
	    cr.Y = sb.dwSize.Y;
	  SetConsoleCursorPosition (h, cr);
	}
      break;

    /* ESC [ n1 'C'
	 Move the cursor right by n1 characters.  */
    case MAKEWORD ('[', 'C'):
      if (esc_head == esc_term)
	n1 = 1;
      else
	{
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  cr = sb.dwCursorPosition;
	  /* Stop at the rightmost boundary.  */
	  if (sb.dwSize.X - cr.X > n1)
	    cr.X += n1;
	  else
	    cr.X = sb.dwSize.X;
	  SetConsoleCursorPosition (h, cr);
	}
      break;

    /* ESC [ n1 'D'
	 Move the cursor left by n1 characters.  */
    case MAKEWORD ('[', 'D'):
      if (esc_head == esc_term)
	n1 = 1;
      else
	{
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  cr = sb.dwCursorPosition;
	  /* Stop at the leftmost boundary.  */
	  if (cr.X > n1)
	    cr.X -= n1;
	  else
	    cr.X = 0;
	  SetConsoleCursorPosition (h, cr);
	}
      break;

    /* ESC [ n1 'E'
	 Move the cursor to the beginning of the n1-th line downwards.  */
    case MAKEWORD ('[', 'E'):
      if (esc_head == esc_term)
	n1 = 1;
      else
	{
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  cr = sb.dwCursorPosition;
	  cr.X = 0;
	  /* Stop at the bottommost boundary.  */
	  if (sb.dwSize.Y - cr.Y > n1)
	    cr.Y += n1;
	  else
	    cr.Y = sb.dwSize.Y;
	  SetConsoleCursorPosition (h, cr);
	}
      break;

    /* ESC [ n1 'F'
	 Move the cursor to the beginning of the n1-th line upwards.  */
    case MAKEWORD ('[', 'F'):
      if (esc_head == esc_term)
	n1 = 1;
      else
	{
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  cr = sb.dwCursorPosition;
	  cr.X = 0;
	  /* Stop at the topmost boundary.  */
	  if (cr.Y > n1)
	    cr.Y -= n1;
	  else
	    cr.Y = 0;
	  SetConsoleCursorPosition (h, cr);
	}
      break;

    /* ESC [ n1 'G'
	 Move the cursor to the (1-based) n1-th column.  */
    case MAKEWORD ('[', 'G'):
      if (esc_head == esc_term)
	n1 = 1;
      else
	{
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  cr = sb.dwCursorPosition;
	  n1 -= 1;
	  /* Stop at the leftmost or rightmost boundary.  */
	  if (n1 < 0)
	    cr.X = 0;
	  else if (n1 > sb.dwSize.X)
	    cr.X = sb.dwSize.X;
	  else
	    cr.X = n1;
	  SetConsoleCursorPosition (h, cr);
	}
      break;

    /* ESC [ n1 ';' n2 'H'
       ESC [ n1 ';' n2 'f'
	 Move the cursor to the (1-based) n1-th row and
	 (also 1-based) n2-th column.  */
    case MAKEWORD ('[', 'H'):
    case MAKEWORD ('[', 'f'):
      if (esc_head == esc_term)
	{
	  /* Both parameters are omitted and set to 1 by default.  */
	  n1 = 1;
	  n2 = 1;
	}
      else if (!(delim = (char *) memchr (esc_head, ';',
					  esc_term - esc_head)))
	{
	  /* Only the first parameter is given.  The second one is
	     set to 1 by default.  */
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	  n2 = 1;
	}
      else
	{
	  /* Both parameters are given.  The first one shall be
	     terminated by the semicolon.  */
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != delim)
	    break;
	  n2 = strtol (delim + 1, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  cr = sb.dwCursorPosition;
	  n1 -= 1;
	  n2 -= 1;
	  /* The cursor position shall be relative to the view coord of
	     the console window, which is usually smaller than the actual
	     buffer.  FWIW, the 'appropriate' solution will be shrinking
	     the buffer to match the size of the console window,
	     destroying scrollback in the process.  */
	  n1 += sb.srWindow.Top;
	  n2 += sb.srWindow.Left;
	  /* Stop at the topmost or bottommost boundary.  */
	  if (n1 < 0)
	    cr.Y = 0;
	  else if (n1 > sb.dwSize.Y)
	    cr.Y = sb.dwSize.Y;
	  else
	    cr.Y = n1;
	  /* Stop at the leftmost or rightmost boundary.  */
	  if (n2 < 0)
	    cr.X = 0;
	  else if (n2 > sb.dwSize.X)
	    cr.X = sb.dwSize.X;
	  else
	    cr.X = n2;
	  SetConsoleCursorPosition (h, cr);
	}
      break;

    /* ESC [ n1 'J'
	 Erase display.  */
    case MAKEWORD ('[', 'J'):
      if (esc_head == esc_term)
	/* This is one of the very few codes whose parameters have
	   a default value of zero.  */
	n1 = 0;
      else
	{
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  /* The cursor is not necessarily in the console window, which
	     makes the behavior of this code harder to define.  */
	  switch (n1)
	    {
	    case 0:
	      /* If the cursor is in or above the window, erase from
		 it to the bottom of the window; otherwise, do nothing.  */
	      cr = sb.dwCursorPosition;
	      cnt = sb.dwSize.X - sb.dwCursorPosition.X;
	      rows = sb.srWindow.Bottom - sb.dwCursorPosition.Y;
	      break;
	    case 1:
	      /* If the cursor is in or under the window, erase from
		 it to the top of the window; otherwise, do nothing.  */
	      cr.X = 0;
	      cr.Y = sb.srWindow.Top;
	      cnt = sb.dwCursorPosition.X + 1;
	      rows = sb.dwCursorPosition.Y - sb.srWindow.Top;
	      break;
	    case 2:
	      /* Erase the entire window.  */
	      cr.X = sb.srWindow.Left;
	      cr.Y = sb.srWindow.Top;
	      cnt = 0;
	      rows = sb.srWindow.Bottom - sb.srWindow.Top + 1;
	      break;
	    default:
	      /* Erase the entire buffer.  */
	      cr.X = 0;
	      cr.Y = 0;
	      cnt = 0;
	      rows = sb.dwSize.Y;
	      break;
	    }
	  if (rows < 0)
	    break;
	  cnt += rows * sb.dwSize.X;
	  FillConsoleOutputCharacterW (h, L' ', cnt, cr, &step);
	  FillConsoleOutputAttribute (h, sb.wAttributes, cnt, cr, &step);
	}
      break;

    /* ESC [ n1 'K'
	 Erase line.  */
    case MAKEWORD ('[', 'K'):
      if (esc_head == esc_term)
	/* This is one of the very few codes whose parameters have
	   a default value of zero.  */
	n1 = 0;
      else
	{
	  n1 = strtol (esc_head, &eptr, 10);
	  if (eptr != esc_term)
	    break;
	}

      if (GetConsoleScreenBufferInfo (h, &sb))
	{
	  switch (n1)
	    {
	    case 0:
	      /* Erase from the cursor to the end.  */
	      cr = sb.dwCursorPosition;
	      cnt = sb.dwSize.X - sb.dwCursorPosition.X;
	      break;
	    case 1:
	      /* Erase from the cursor to the beginning.  */
	      cr = sb.dwCursorPosition;
	      cr.X = 0;
	      cnt = sb.dwCursorPosition.X + 1;
	      break;
	    default:
	      /* Erase the entire line.  */
	      cr = sb.dwCursorPosition;
	      cr.X = 0;
	      cnt = sb.dwSize.X;
	      break;
	    }
	  FillConsoleOutputCharacterW (h, L' ', cnt, cr, &step);
	  FillConsoleOutputAttribute (h, sb.wAttributes, cnt, cr, &step);
	}
      break;

    /* ESC [ n1 ';' n2 'm'
	 Set SGR parameters.  Zero or more parameters will follow.  */
    case MAKEWORD ('[', 'm'):
      attrib_add = 0;
      attrib_rm = 0;
      if (esc_head == esc_term)
	{
	  /* When no parameter is given, reset the console.  */
	  attrib_add |= (FOREGROUND_RED | FOREGROUND_GREEN
			 | FOREGROUND_BLUE);
	  attrib_rm = -1; /* Removes everything.  */
	  goto sgr_set_it;
	}
      param = esc_head;
      do
	{
	  /* Parse a parameter.  */
	  n1 = strtol (param, &eptr, 10);
	  if (*eptr != ';' && eptr != esc_term)
	    goto sgr_set_it;

	  switch (n1)
	    {
	    case 0:
	      /* Reset.  */
	      attrib_add |= (FOREGROUND_RED | FOREGROUND_GREEN
			     | FOREGROUND_BLUE);
	      attrib_rm = -1; /* Removes everything.  */
	      break;
	    case 1:
	      /* Bold.  */
	      attrib_add |= FOREGROUND_INTENSITY;
	      break;
	    case 4:
	      /* Underline.  */
	      attrib_add |= COMMON_LVB_UNDERSCORE;
	      break;
	    case 5:
	      /* Blink.  */
	      /* XXX: It is not BLINKING at all! */
	      attrib_add |= BACKGROUND_INTENSITY;
	      break;
	    case 7:
	      /* Reverse.  */
	      attrib_add |= COMMON_LVB_REVERSE_VIDEO;
	      break;
	    case 22:
	      /* No bold.  */
	      attrib_add &= ~FOREGROUND_INTENSITY;
	      attrib_rm |= FOREGROUND_INTENSITY;
	      break;
	    case 24:
	      /* No underline.  */
	      attrib_add &= ~COMMON_LVB_UNDERSCORE;
	      attrib_rm |= COMMON_LVB_UNDERSCORE;
	      break;
	    case 25:
	      /* No blink.  */
	      /* XXX: It is not BLINKING at all! */
	      attrib_add &= ~BACKGROUND_INTENSITY;
	      attrib_rm |= BACKGROUND_INTENSITY;
	      break;
	    case 27:
	      /* No reverse.  */
	      attrib_add &= ~COMMON_LVB_REVERSE_VIDEO;
	      attrib_rm |= COMMON_LVB_REVERSE_VIDEO;
	      break;
	    case 30:
	    case 31:
	    case 32:
	    case 33:
	    case 34:
	    case 35:
	    case 36:
	    case 37:
	      /* Foreground color.  */
	      attrib_add &= ~(FOREGROUND_RED | FOREGROUND_GREEN
			      | FOREGROUND_BLUE);
	      n1 -= 30;
	      if (n1 & 1)
		attrib_add |= FOREGROUND_RED;
	      if (n1 & 2)
		attrib_add |= FOREGROUND_GREEN;
	      if (n1 & 4)
		attrib_add |= FOREGROUND_BLUE;
	      attrib_rm |= (FOREGROUND_RED | FOREGROUND_GREEN
			    | FOREGROUND_BLUE);
	      break;
	    case 38:
	      /* Reserved for extended foreground color.
		 Don't know how to handle parameters remaining.
		 Bail out.  */
	      goto sgr_set_it;
	    case 39:
	      /* Reset foreground color.  */
	      /* Set to grey.  */
	      attrib_add |= (FOREGROUND_RED | FOREGROUND_GREEN
			     | FOREGROUND_BLUE);
	      attrib_rm |= (FOREGROUND_RED | FOREGROUND_GREEN
			    | FOREGROUND_BLUE);
	      break;
	    case 40:
	    case 41:
	    case 42:
	    case 43:
	    case 44:
	    case 45:
	    case 46:
	    case 47:
	      /* Background color.  */
	      attrib_add &= ~(BACKGROUND_RED | BACKGROUND_GREEN
			      | BACKGROUND_BLUE);
	      n1 -= 40;
	      if (n1 & 1)
		attrib_add |= BACKGROUND_RED;
	      if (n1 & 2)
		attrib_add |= BACKGROUND_GREEN;
	      if (n1 & 4)
		attrib_add |= BACKGROUND_BLUE;
	      attrib_rm |= (BACKGROUND_RED | BACKGROUND_GREEN
			    | BACKGROUND_BLUE);
	      break;
	    case 48:
	      /* Reserved for extended background color.
		 Don't know how to handle parameters remaining.
		 Bail out.  */
	      goto sgr_set_it;
	    case 49:
	      /* Reset background color.  */
	      /* Set to black.  */
	      attrib_add &= ~(BACKGROUND_RED | BACKGROUND_GREEN
			      | BACKGROUND_BLUE);
	      attrib_rm |= (BACKGROUND_RED | BACKGROUND_GREEN
			    | BACKGROUND_BLUE);
	      break;
	    }

	  /* Prepare the next parameter.  */
	  param = eptr + 1;
	}
      while (param != esc_term);

sgr_set_it:
      /* 0xFFFF removes everything.  If it is not the case,
	 care must be taken to preserve old attributes.  */
      if (attrib_rm != 0xFFFF && GetConsoleScreenBufferInfo (h, &sb))
	{
	  attrib_add |= sb.wAttributes & ~attrib_rm;
	}
      SetConsoleTextAttribute (h, attrib_add);
      break;
    }
}

int
mingw_ansi_fputs (const char *str, FILE *fp)
{
  const char *read = str;
  HANDLE h;
  DWORD mode;
  int esc_code, prefix_len;
  const char *esc_head, *esc_term;

  h = (HANDLE) _get_osfhandle (_fileno (fp));
  if (h == INVALID_HANDLE_VALUE)
    return EOF;

  /* Don't mess up stdio functions with Windows APIs.  */
  fflush (fp);

  if (GetConsoleMode (h, &mode))
    /* If it is a console, translate ANSI escape codes as needed.  */
    for (;;)
      {
	if ((esc_code = find_esc_head (&prefix_len, &esc_head, read)) == 0)
	  {
	    /* Write all remaining characters, then exit.  */
	    write_all (h, read, strlen (read));
	    break;
	  }
	if (find_esc_terminator (&esc_term, esc_head) == 0)
	  /* Ignore incomplete escape sequences at the moment.
	     FIXME: The escape state shall be cached for further calls
		    to this function.  */
	  break;
	write_all (h, read, esc_head - prefix_len - read);
	eat_esc_sequence (h, esc_code, esc_head, esc_term);
	read = esc_term + 1;
      }
  else
    /* If it is not a console, write everything as-is.  */
    write_all (h, read, strlen (read));

  _close ((intptr_t) h);
  return 1;
}

#endif /* __MINGW32__ */

static void pp_quoted_string (pretty_printer *, const char *, size_t = -1);

/* Overwrite the given location/range within this text_info's rich_location.
   For use e.g. when implementing "+" in client format decoders.  */

void
text_info::set_location (unsigned int idx, location_t loc, bool show_caret_p)
{
  gcc_checking_assert (m_richloc);
  m_richloc->set_range (idx, loc, show_caret_p);
}

location_t
text_info::get_location (unsigned int index_of_location) const
{
  gcc_checking_assert (m_richloc);

  if (index_of_location == 0)
    return m_richloc->get_loc ();
  else
    return UNKNOWN_LOCATION;
}

// Default construct an output buffer.

output_buffer::output_buffer ()
  : formatted_obstack (),
    chunk_obstack (),
    obstack (&formatted_obstack),
    cur_chunk_array (),
    stream (stderr),
    line_length (),
    digit_buffer (),
    flush_p (true)
{
  obstack_init (&formatted_obstack);
  obstack_init (&chunk_obstack);
}

// Release resources owned by an output buffer at the end of lifetime.

output_buffer::~output_buffer ()
{
  obstack_free (&chunk_obstack, NULL);
  obstack_free (&formatted_obstack, NULL);
}


/* Format an integer given by va_arg (ARG, type-specifier T) where
   type-specifier is a precision modifier as indicated by PREC.  F is
   a string used to construct the appropriate format-specifier.  */
#define pp_integer_with_precision(PP, ARG, PREC, T, F)       \
  do                                                         \
    switch (PREC)                                            \
      {                                                      \
      case 0:                                                \
        pp_scalar (PP, "%" F, va_arg (ARG, T));              \
        break;                                               \
                                                             \
      case 1:                                                \
        pp_scalar (PP, "%l" F, va_arg (ARG, long T));        \
        break;                                               \
                                                             \
      case 2:                                                \
        pp_scalar (PP, "%" HOST_LONG_LONG_FORMAT F, va_arg (ARG, long long T));  \
        break;                                               \
                                                             \
      default:                                               \
        break;                                               \
      }                                                      \
  while (0)


/* Subroutine of pp_set_maximum_length.  Set up PRETTY-PRINTER's
   internal maximum characters per line.  */
static void
pp_set_real_maximum_length (pretty_printer *pp)
{
  /* If we're told not to wrap lines then do the obvious thing.  In case
     we'll emit prefix only once per message, it is appropriate
     not to increase unnecessarily the line-length cut-off.  */
  if (!pp_is_wrapping_line (pp)
      || pp_prefixing_rule (pp) == DIAGNOSTICS_SHOW_PREFIX_ONCE
      || pp_prefixing_rule (pp) == DIAGNOSTICS_SHOW_PREFIX_NEVER)
    pp->maximum_length = pp_line_cutoff (pp);
  else
    {
      int prefix_length = pp->prefix ? strlen (pp->prefix) : 0;
      /* If the prefix is ridiculously too long, output at least
         32 characters.  */
      if (pp_line_cutoff (pp) - prefix_length < 32)
	pp->maximum_length = pp_line_cutoff (pp) + 32;
      else
	pp->maximum_length = pp_line_cutoff (pp);
    }
}

/* Clear PRETTY-PRINTER's output state.  */
static inline void
pp_clear_state (pretty_printer *pp)
{
  pp->emitted_prefix = false;
  pp_indentation (pp) = 0;
}

/* Print X to PP in decimal.  */
template<unsigned int N, typename T>
void
pp_wide_integer (pretty_printer *pp, const poly_int_pod<N, T> &x)
{
  if (x.is_constant ())
    pp_wide_integer (pp, x.coeffs[0]);
  else
    {
      pp_left_bracket (pp);
      for (unsigned int i = 0; i < N; ++i)
	{
	  if (i != 0)
	    pp_comma (pp);
	  pp_wide_integer (pp, x.coeffs[i]);
	}
      pp_right_bracket (pp);
    }
}

template void pp_wide_integer (pretty_printer *, const poly_uint16_pod &);
template void pp_wide_integer (pretty_printer *, const poly_int64_pod &);
template void pp_wide_integer (pretty_printer *, const poly_uint64_pod &);

/* Flush the formatted text of PRETTY-PRINTER onto the attached stream.  */
void
pp_write_text_to_stream (pretty_printer *pp)
{
  const char *text = pp_formatted_text (pp);
#ifdef __MINGW32__
  mingw_ansi_fputs (text, pp_buffer (pp)->stream);
#else
  fputs (text, pp_buffer (pp)->stream);
#endif
  pp_clear_output_area (pp);
}

/* As pp_write_text_to_stream, but for GraphViz label output.

   Flush the formatted text of pretty-printer PP onto the attached stream.
   Replace characters in PPF that have special meaning in a GraphViz .dot
   file.
   
   This routine is not very fast, but it doesn't have to be as this is only
   be used by routines dumping intermediate representations in graph form.  */

void
pp_write_text_as_dot_label_to_stream (pretty_printer *pp, bool for_record)
{
  const char *text = pp_formatted_text (pp);
  const char *p = text;
  FILE *fp = pp_buffer (pp)->stream;

  for (;*p; p++)
    {
      bool escape_char;
      switch (*p)
	{
	/* Print newlines as a left-aligned newline.  */
	case '\n':
	  fputs ("\\l", fp);
	  escape_char = true;
	  break;

	/* The following characters are only special for record-shape nodes.  */
	case '|':
	case '{':
	case '}':
	case '<':
	case '>':
	case ' ':
	  escape_char = for_record;
	  break;

	/* The following characters always have to be escaped
	   for use in labels.  */
	case '\\':
	  /* There is a bug in some (f.i. 2.36.0) versions of graphiz
	     ( http://www.graphviz.org/mantisbt/view.php?id=2524 ) related to
	     backslash as last char in label.  Let's avoid triggering it.  */
	  gcc_assert (*(p + 1) != '\0');
	  /* Fall through.  */
	case '"':
	  escape_char = true;
	  break;

	default:
	  escape_char = false;
	  break;
	}

      if (escape_char)
	fputc ('\\', fp);

      fputc (*p, fp);
    }

  pp_clear_output_area (pp);
}

/* Wrap a text delimited by START and END into PRETTY-PRINTER.  */
static void
pp_wrap_text (pretty_printer *pp, const char *start, const char *end)
{
  bool wrapping_line = pp_is_wrapping_line (pp);

  while (start != end)
    {
      /* Dump anything bordered by whitespaces.  */
      {
	const char *p = start;
	while (p != end && !ISBLANK (*p) && *p != '\n')
	  ++p;
	if (wrapping_line
            && p - start >= pp_remaining_character_count_for_line (pp))
	  pp_newline (pp);
	pp_append_text (pp, start, p);
	start = p;
      }

      if (start != end && ISBLANK (*start))
	{
	  pp_space (pp);
	  ++start;
	}
      if (start != end && *start == '\n')
	{
	  pp_newline (pp);
	  ++start;
	}
    }
}

/* Same as pp_wrap_text but wrap text only when in line-wrapping mode.  */
static inline void
pp_maybe_wrap_text (pretty_printer *pp, const char *start, const char *end)
{
  if (pp_is_wrapping_line (pp))
    pp_wrap_text (pp, start, end);
  else
    pp_append_text (pp, start, end);
}

/* Append to the output area of PRETTY-PRINTER a string specified by its
   STARTing character and LENGTH.  */
static inline void
pp_append_r (pretty_printer *pp, const char *start, int length)
{
  output_buffer_append_r (pp_buffer (pp), start, length);
}

/* Insert enough spaces into the output area of PRETTY-PRINTER to bring
   the column position to the current indentation level, assuming that a
   newline has just been written to the buffer.  */
void
pp_indent (pretty_printer *pp)
{
  int n = pp_indentation (pp);
  int i;

  for (i = 0; i < n; ++i)
    pp_space (pp);
}

/* The following format specifiers are recognized as being client independent:
   %d, %i: (signed) integer in base ten.
   %u: unsigned integer in base ten.
   %o: unsigned integer in base eight.
   %x: unsigned integer in base sixteen.
   %ld, %li, %lo, %lu, %lx: long versions of the above.
   %lld, %lli, %llo, %llu, %llx: long long versions.
   %wd, %wi, %wo, %wu, %wx: HOST_WIDE_INT versions.
   %c: character.
   %s: string.
   %p: pointer (printed in a host-dependent manner).
   %r: if pp_show_color(pp), switch to color identified by const char *.
   %R: if pp_show_color(pp), reset color.
   %m: strerror(text->err_no) - does not consume a value from args_ptr.
   %%: '%'.
   %<: opening quote.
   %>: closing quote.
   %': apostrophe (should only be used in untranslated messages;
       translations should use appropriate punctuation directly).
   %.*s: a substring the length of which is specified by an argument
	 integer.
   %Ns: likewise, but length specified as constant in the format string.
   Flag 'q': quote formatted text (must come immediately after '%').
   %Z: Requires two arguments - array of int, and len. Prints elements
   of the array.

   Arguments can be used sequentially, or through %N$ resp. *N$
   notation Nth argument after the format string.  If %N$ / *N$
   notation is used, it must be used for all arguments, except %m, %%,
   %<, %> and %', which may not have a number, as they do not consume
   an argument.  When %M$.*N$s is used, M must be N + 1.  (This may
   also be written %M$.*s, provided N is not otherwise used.)  The
   format string must have conversion specifiers with argument numbers
   1 up to highest argument; each argument may only be used once.
   A format string can have at most 30 arguments.  */

/* Formatting phases 1 and 2: render TEXT->format_spec plus
   TEXT->args_ptr into a series of chunks in pp_buffer (PP)->args[].
   Phase 3 is in pp_output_formatted_text.  */

void
pp_format (pretty_printer *pp, text_info *text)
{
  output_buffer *buffer = pp_buffer (pp);
  const char *p;
  const char **args;
  struct chunk_info *new_chunk_array;

  unsigned int curarg = 0, chunk = 0, argno;
  pp_wrapping_mode_t old_wrapping_mode;
  bool any_unnumbered = false, any_numbered = false;
  const char **formatters[PP_NL_ARGMAX];

  /* Allocate a new chunk structure.  */
  new_chunk_array = XOBNEW (&buffer->chunk_obstack, struct chunk_info);
  new_chunk_array->prev = buffer->cur_chunk_array;
  buffer->cur_chunk_array = new_chunk_array;
  args = new_chunk_array->args;

  /* Formatting phase 1: split up TEXT->format_spec into chunks in
     pp_buffer (PP)->args[].  Even-numbered chunks are to be output
     verbatim, odd-numbered chunks are format specifiers.
     %m, %%, %<, %>, and %' are replaced with the appropriate text at
     this point.  */

  memset (formatters, 0, sizeof formatters);

  for (p = text->format_spec; *p; )
    {
      while (*p != '\0' && *p != '%')
	{
	  obstack_1grow (&buffer->chunk_obstack, *p);
	  p++;
	}

      if (*p == '\0')
	break;

      switch (*++p)
	{
	case '\0':
	  gcc_unreachable ();

	case '%':
	  obstack_1grow (&buffer->chunk_obstack, '%');
	  p++;
	  continue;

	case '<':
	  {
	    obstack_grow (&buffer->chunk_obstack,
			  open_quote, strlen (open_quote));
	    const char *colorstr
	      = colorize_start (pp_show_color (pp), "quote");
	    obstack_grow (&buffer->chunk_obstack, colorstr, strlen (colorstr));
	    p++;
	    continue;
	  }

	case '>':
	  {
	    const char *colorstr = colorize_stop (pp_show_color (pp));
	    obstack_grow (&buffer->chunk_obstack, colorstr, strlen (colorstr));
	  }
	  /* FALLTHRU */
	case '\'':
	  obstack_grow (&buffer->chunk_obstack,
			close_quote, strlen (close_quote));
	  p++;
	  continue;

	case 'R':
	  {
	    const char *colorstr = colorize_stop (pp_show_color (pp));
	    obstack_grow (&buffer->chunk_obstack, colorstr,
			  strlen (colorstr));
	    p++;
	    continue;
	  }

	case 'm':
	  {
	    const char *errstr = xstrerror (text->err_no);
	    obstack_grow (&buffer->chunk_obstack, errstr, strlen (errstr));
	  }
	  p++;
	  continue;

	default:
	  /* Handled in phase 2.  Terminate the plain chunk here.  */
	  obstack_1grow (&buffer->chunk_obstack, '\0');
	  gcc_assert (chunk < PP_NL_ARGMAX * 2);
	  args[chunk++] = XOBFINISH (&buffer->chunk_obstack, const char *);
	  break;
	}

      if (ISDIGIT (*p))
	{
	  char *end;
	  argno = strtoul (p, &end, 10) - 1;
	  p = end;
	  gcc_assert (*p == '$');
	  p++;

	  any_numbered = true;
	  gcc_assert (!any_unnumbered);
	}
      else
	{
	  argno = curarg++;
	  any_unnumbered = true;
	  gcc_assert (!any_numbered);
	}
      gcc_assert (argno < PP_NL_ARGMAX);
      gcc_assert (!formatters[argno]);
      formatters[argno] = &args[chunk];
      do
	{
	  obstack_1grow (&buffer->chunk_obstack, *p);
	  p++;
	}
      while (strchr ("qwl+#", p[-1]));

      if (p[-1] == '.')
	{
	  /* We handle '%.Ns' and '%.*s' or '%M$.*N$s'
	     (where M == N + 1).  */
	  if (ISDIGIT (*p))
	    {
	      do
		{
		  obstack_1grow (&buffer->chunk_obstack, *p);
		  p++;
		}
	      while (ISDIGIT (p[-1]));
	      gcc_assert (p[-1] == 's');
	    }
	  else
	    {
	      gcc_assert (*p == '*');
	      obstack_1grow (&buffer->chunk_obstack, '*');
	      p++;

	      if (ISDIGIT (*p))
		{
		  char *end;
		  unsigned int argno2 = strtoul (p, &end, 10) - 1;
		  p = end;
		  gcc_assert (argno2 == argno - 1);
		  gcc_assert (!any_unnumbered);
		  gcc_assert (*p == '$');

		  p++;
		  formatters[argno2] = formatters[argno];
		}
	      else
		{
		  gcc_assert (!any_numbered);
		  formatters[argno+1] = formatters[argno];
		  curarg++;
		}
	      gcc_assert (*p == 's');
	      obstack_1grow (&buffer->chunk_obstack, 's');
	      p++;
	    }
	}
      if (*p == '\0')
	break;

      obstack_1grow (&buffer->chunk_obstack, '\0');
      gcc_assert (chunk < PP_NL_ARGMAX * 2);
      args[chunk++] = XOBFINISH (&buffer->chunk_obstack, const char *);
    }

  obstack_1grow (&buffer->chunk_obstack, '\0');
  gcc_assert (chunk < PP_NL_ARGMAX * 2);
  args[chunk++] = XOBFINISH (&buffer->chunk_obstack, const char *);
  args[chunk] = 0;

  /* Set output to the argument obstack, and switch line-wrapping and
     prefixing off.  */
  buffer->obstack = &buffer->chunk_obstack;
  old_wrapping_mode = pp_set_verbatim_wrapping (pp);

  /* Second phase.  Replace each formatter with the formatted text it
     corresponds to.  */

  for (argno = 0; formatters[argno]; argno++)
    {
      int precision = 0;
      bool wide = false;
      bool plus = false;
      bool hash = false;
      bool quote = false;

      /* We do not attempt to enforce any ordering on the modifier
	 characters.  */

      for (p = *formatters[argno];; p++)
	{
	  switch (*p)
	    {
	    case 'q':
	      gcc_assert (!quote);
	      quote = true;
	      continue;

	    case '+':
	      gcc_assert (!plus);
	      plus = true;
	      continue;

	    case '#':
	      gcc_assert (!hash);
	      hash = true;
	      continue;

	    case 'w':
	      gcc_assert (!wide);
	      wide = true;
	      continue;

	    case 'l':
	      /* We don't support precision beyond that of "long long".  */
	      gcc_assert (precision < 2);
	      precision++;
	      continue;
	    }
	  break;
	}

      gcc_assert (!wide || precision == 0);

      if (quote)
	pp_begin_quote (pp, pp_show_color (pp));

      switch (*p)
	{
	case 'r':
	  pp_string (pp, colorize_start (pp_show_color (pp),
					 va_arg (*text->args_ptr,
						 const char *)));
	  break;

	case 'c':
	  {
	    /* When quoting, print alphanumeric, punctuation, and the space
	       character unchanged, and all others in hexadecimal with the
	       "\x" prefix.  Otherwise print them all unchanged.  */
	    int chr = va_arg (*text->args_ptr, int);
	    if (ISPRINT (chr) || !quote)
	      pp_character (pp, chr);
	    else
	      {
		const char str [2] = { chr, '\0' };
		pp_quoted_string (pp, str, 1);
	      }
	    break;
	  }

	case 'd':
	case 'i':
	  if (wide)
	    pp_wide_integer (pp, va_arg (*text->args_ptr, HOST_WIDE_INT));
	  else
	    pp_integer_with_precision
	      (pp, *text->args_ptr, precision, int, "d");
	  break;

	case 'o':
	  if (wide)
	    pp_scalar (pp, "%" HOST_WIDE_INT_PRINT "o",
		       va_arg (*text->args_ptr, unsigned HOST_WIDE_INT));
	  else
	    pp_integer_with_precision
	      (pp, *text->args_ptr, precision, unsigned, "o");
	  break;

	case 's':
	  if (quote)
	    pp_quoted_string (pp, va_arg (*text->args_ptr, const char *));
	  else
	    pp_string (pp, va_arg (*text->args_ptr, const char *));
	  break;

	case 'p':
	  pp_pointer (pp, va_arg (*text->args_ptr, void *));
	  break;

	case 'u':
	  if (wide)
	    pp_scalar (pp, HOST_WIDE_INT_PRINT_UNSIGNED,
		       va_arg (*text->args_ptr, unsigned HOST_WIDE_INT));
	  else
	    pp_integer_with_precision
	      (pp, *text->args_ptr, precision, unsigned, "u");
	  break;

	case 'Z':
	  {
	    int *v = va_arg (*text->args_ptr, int *);
	    unsigned len = va_arg (*text->args_ptr, unsigned); 

	    for (unsigned i = 0; i < len; ++i)
	      {
		pp_scalar (pp, "%i", v[i]);
		if (i < len - 1)
		  {
		    pp_comma (pp);
		    pp_space (pp);
		  }
	      }
	    break;
	 }

	case 'x':
	  if (wide)
	    pp_scalar (pp, HOST_WIDE_INT_PRINT_HEX,
		       va_arg (*text->args_ptr, unsigned HOST_WIDE_INT));
	  else
	    pp_integer_with_precision
	      (pp, *text->args_ptr, precision, unsigned, "x");
	  break;

	case '.':
	  {
	    int n;
	    const char *s;

	    /* We handle '%.Ns' and '%.*s' or '%M$.*N$s'
	       (where M == N + 1).  The format string should be verified
	       already from the first phase.  */
	    p++;
	    if (ISDIGIT (*p))
	      {
		char *end;
		n = strtoul (p, &end, 10);
		p = end;
		gcc_assert (*p == 's');
	      }
	    else
	      {
		gcc_assert (*p == '*');
		p++;
		gcc_assert (*p == 's');
		n = va_arg (*text->args_ptr, int);

		/* This consumes a second entry in the formatters array.  */
		gcc_assert (formatters[argno] == formatters[argno+1]);
		argno++;
	      }

	    s = va_arg (*text->args_ptr, const char *);

	    /* Append the lesser of precision and strlen (s) characters
	       from the array (which need not be a nul-terminated string).
	       Negative precision is treated as if it were omitted.  */
	    size_t len = n < 0 ? strlen (s) : strnlen (s, n);

	    pp_append_text (pp, s, s + len);
	  }
	  break;

	default:
	  {
	    bool ok;

	    /* Call the format decoder.
	       Pass the address of "quote" so that format decoders can
	       potentially disable printing of the closing quote
	       (e.g. when printing "'TYPEDEF' aka 'TYPE'" in the C family
	       of frontends).  */
	    gcc_assert (pp_format_decoder (pp));
	    ok = pp_format_decoder (pp) (pp, text, p,
					 precision, wide, plus, hash, &quote,
					 formatters[argno]);
	    gcc_assert (ok);
	  }
	}

      if (quote)
	pp_end_quote (pp, pp_show_color (pp));

      obstack_1grow (&buffer->chunk_obstack, '\0');
      *formatters[argno] = XOBFINISH (&buffer->chunk_obstack, const char *);
    }

  if (CHECKING_P)
    for (; argno < PP_NL_ARGMAX; argno++)
      gcc_assert (!formatters[argno]);

  /* If the client supplied a postprocessing object, call its "handle"
     hook here.  */
  if (pp->m_format_postprocessor)
    pp->m_format_postprocessor->handle (pp);

  /* Revert to normal obstack and wrapping mode.  */
  buffer->obstack = &buffer->formatted_obstack;
  buffer->line_length = 0;
  pp_wrapping_mode (pp) = old_wrapping_mode;
  pp_clear_state (pp);
}

/* Format of a message pointed to by TEXT.  */
void
pp_output_formatted_text (pretty_printer *pp)
{
  unsigned int chunk;
  output_buffer *buffer = pp_buffer (pp);
  struct chunk_info *chunk_array = buffer->cur_chunk_array;
  const char **args = chunk_array->args;

  gcc_assert (buffer->obstack == &buffer->formatted_obstack);
  gcc_assert (buffer->line_length == 0);

  /* This is a third phase, first 2 phases done in pp_format_args.
     Now we actually print it.  */
  for (chunk = 0; args[chunk]; chunk++)
    pp_string (pp, args[chunk]);

  /* Deallocate the chunk structure and everything after it (i.e. the
     associated series of formatted strings).  */
  buffer->cur_chunk_array = chunk_array->prev;
  obstack_free (&buffer->chunk_obstack, chunk_array);
}

/* Helper subroutine of output_verbatim and verbatim. Do the appropriate
   settings needed by BUFFER for a verbatim formatting.  */
void
pp_format_verbatim (pretty_printer *pp, text_info *text)
{
  /* Set verbatim mode.  */
  pp_wrapping_mode_t oldmode = pp_set_verbatim_wrapping (pp);

  /* Do the actual formatting.  */
  pp_format (pp, text);
  pp_output_formatted_text (pp);

  /* Restore previous settings.  */
  pp_wrapping_mode (pp) = oldmode;
}

/* Flush the content of BUFFER onto the attached stream.  This
   function does nothing unless pp->output_buffer->flush_p.  */
void
pp_flush (pretty_printer *pp)
{
  pp_clear_state (pp);
  if (!pp->buffer->flush_p)
    return;
  pp_write_text_to_stream (pp);
  fflush (pp_buffer (pp)->stream);
}

/* Flush the content of BUFFER onto the attached stream independently
   of the value of pp->output_buffer->flush_p.  */
void
pp_really_flush (pretty_printer *pp)
{
  pp_clear_state (pp);
  pp_write_text_to_stream (pp);
  fflush (pp_buffer (pp)->stream);
}

/* Sets the number of maximum characters per line PRETTY-PRINTER can
   output in line-wrapping mode.  A LENGTH value 0 suppresses
   line-wrapping.  */
void
pp_set_line_maximum_length (pretty_printer *pp, int length)
{
  pp_line_cutoff (pp) = length;
  pp_set_real_maximum_length (pp);
}

/* Clear PRETTY-PRINTER output area text info.  */
void
pp_clear_output_area (pretty_printer *pp)
{
  obstack_free (pp_buffer (pp)->obstack,
                obstack_base (pp_buffer (pp)->obstack));
  pp_buffer (pp)->line_length = 0;
}

/* Set PREFIX for PRETTY-PRINTER.  */
void
pp_set_prefix (pretty_printer *pp, const char *prefix)
{
  pp->prefix = prefix;
  pp_set_real_maximum_length (pp);
  pp->emitted_prefix = false;
  pp_indentation (pp) = 0;
}

/* Free PRETTY-PRINTER's prefix, a previously malloc()'d string.  */
void
pp_destroy_prefix (pretty_printer *pp)
{
  if (pp->prefix != NULL)
    {
      free (CONST_CAST (char *, pp->prefix));
      pp->prefix = NULL;
    }
}

/* Write out PRETTY-PRINTER's prefix.  */
void
pp_emit_prefix (pretty_printer *pp)
{
  if (pp->prefix != NULL)
    {
      switch (pp_prefixing_rule (pp))
	{
	default:
	case DIAGNOSTICS_SHOW_PREFIX_NEVER:
	  break;

	case DIAGNOSTICS_SHOW_PREFIX_ONCE:
	  if (pp->emitted_prefix)
	    {
	      pp_indent (pp);
	      break;
	    }
	  pp_indentation (pp) += 3;
	  /* Fall through.  */

	case DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE:
	  {
	    int prefix_length = strlen (pp->prefix);
	    pp_append_r (pp, pp->prefix, prefix_length);
	    pp->emitted_prefix = true;
	  }
	  break;
	}
    }
}

/* Construct a PRETTY-PRINTER with PREFIX and of MAXIMUM_LENGTH
   characters per line.  */

pretty_printer::pretty_printer (const char *p, int l)
  : buffer (new (XCNEW (output_buffer)) output_buffer ()),
    prefix (),
    padding (pp_none),
    maximum_length (),
    indent_skip (),
    wrapping (),
    format_decoder (),
    m_format_postprocessor (NULL),
    emitted_prefix (),
    need_newline (),
    translate_identifiers (true),
    show_color ()
{
  pp_line_cutoff (this) = l;
  /* By default, we emit prefixes once per message.  */
  pp_prefixing_rule (this) = DIAGNOSTICS_SHOW_PREFIX_ONCE;
  pp_set_prefix (this, p);
}

pretty_printer::~pretty_printer ()
{
  if (m_format_postprocessor)
    delete m_format_postprocessor;
  buffer->~output_buffer ();
  XDELETE (buffer);
}

/* Append a string delimited by START and END to the output area of
   PRETTY-PRINTER.  No line wrapping is done.  However, if beginning a
   new line then emit PRETTY-PRINTER's prefix and skip any leading
   whitespace if appropriate.  The caller must ensure that it is
   safe to do so.  */
void
pp_append_text (pretty_printer *pp, const char *start, const char *end)
{
  /* Emit prefix and skip whitespace if we're starting a new line.  */
  if (pp_buffer (pp)->line_length == 0)
    {
      pp_emit_prefix (pp);
      if (pp_is_wrapping_line (pp))
	while (start != end && *start == ' ')
	  ++start;
    }
  pp_append_r (pp, start, end - start);
}

/* Finishes constructing a NULL-terminated character string representing
   the PRETTY-PRINTED text.  */
const char *
pp_formatted_text (pretty_printer *pp)
{
  return output_buffer_formatted_text (pp_buffer (pp));
}

/*  Return a pointer to the last character emitted in PRETTY-PRINTER's
    output area.  A NULL pointer means no character available.  */
const char *
pp_last_position_in_text (const pretty_printer *pp)
{
  return output_buffer_last_position_in_text (pp_buffer (pp));
}

/* Return the amount of characters PRETTY-PRINTER can accept to
   make a full line.  Meaningful only in line-wrapping mode.  */
int
pp_remaining_character_count_for_line (pretty_printer *pp)
{
  return pp->maximum_length - pp_buffer (pp)->line_length;
}


/* Format a message into BUFFER a la printf.  */
void
pp_printf (pretty_printer *pp, const char *msg, ...)
{
  text_info text;
  va_list ap;

  va_start (ap, msg);
  text.err_no = errno;
  text.args_ptr = &ap;
  text.format_spec = msg;
  pp_format (pp, &text);
  pp_output_formatted_text (pp);
  va_end (ap);
}


/* Output MESSAGE verbatim into BUFFER.  */
void
pp_verbatim (pretty_printer *pp, const char *msg, ...)
{
  text_info text;
  va_list ap;

  va_start (ap, msg);
  text.err_no = errno;
  text.args_ptr = &ap;
  text.format_spec = msg;
  pp_format_verbatim (pp, &text);
  va_end (ap);
}



/* Have PRETTY-PRINTER start a new line.  */
void
pp_newline (pretty_printer *pp)
{
  obstack_1grow (pp_buffer (pp)->obstack, '\n');
  pp_needs_newline (pp) = false;
  pp_buffer (pp)->line_length = 0;
}

/* Have PRETTY-PRINTER add a CHARACTER.  */
void
pp_character (pretty_printer *pp, int c)
{
  if (pp_is_wrapping_line (pp)
      && pp_remaining_character_count_for_line (pp) <= 0)
    {
      pp_newline (pp);
      if (ISSPACE (c))
        return;
    }
  obstack_1grow (pp_buffer (pp)->obstack, c);
  ++pp_buffer (pp)->line_length;
}

/* Append a STRING to the output area of PRETTY-PRINTER; the STRING may
   be line-wrapped if in appropriate mode.  */
void
pp_string (pretty_printer *pp, const char *str)
{
  gcc_checking_assert (str);
  pp_maybe_wrap_text (pp, str, str + strlen (str));
}

/* Append the leading N characters of STRING to the output area of
   PRETTY-PRINTER, quoting in hexadecimal non-printable characters.
   Setting N = -1 is as if N were set to strlen (STRING).  The STRING
   may be line-wrapped if in appropriate mode.  */
static void
pp_quoted_string (pretty_printer *pp, const char *str, size_t n /* = -1 */)
{
  gcc_checking_assert (str);

  const char *last = str;
  const char *ps;

  /* Compute the length if not specified.  */
  if (n == (size_t) -1)
    n = strlen (str);

  for (ps = str; n; ++ps, --n)
    {
      if (ISPRINT (*ps))
	  continue;

      if (last < ps)
	pp_maybe_wrap_text (pp, last, ps - 1);

      /* Append the hexadecimal value of the character.  Allocate a buffer
	 that's large enough for a 32-bit char plus the hex prefix.  */
      char buf [11];
      int n = sprintf (buf, "\\x%02x", (unsigned char)*ps);
      pp_maybe_wrap_text (pp, buf, buf + n);
      last = ps + 1;
    }

  pp_maybe_wrap_text (pp, last, ps);
}

/* Maybe print out a whitespace if needed.  */

void
pp_maybe_space (pretty_printer *pp)
{
  if (pp->padding != pp_none)
    {
      pp_space (pp);
      pp->padding = pp_none;
    }
}

// Add a newline to the pretty printer PP and flush formatted text.

void
pp_newline_and_flush (pretty_printer *pp)
{
  pp_newline (pp);
  pp_flush (pp);
  pp_needs_newline (pp) = false;
}

// Add a newline to the pretty printer PP, followed by indentation.

void
pp_newline_and_indent (pretty_printer *pp, int n)
{
  pp_indentation (pp) += n;
  pp_newline (pp);
  pp_indent (pp);
  pp_needs_newline (pp) = false;
}

// Add separator C, followed by a single whitespace.

void
pp_separate_with (pretty_printer *pp, char c)
{
  pp_character (pp, c);
  pp_space (pp);
}

/* Add a localized open quote, and if SHOW_COLOR is true, begin colorizing
   using the "quote" color.  */

void
pp_begin_quote (pretty_printer *pp, bool show_color)
{
  pp_string (pp, open_quote);
  pp_string (pp, colorize_start (show_color, "quote"));
}

/* If SHOW_COLOR is true, stop colorizing.
   Add a localized close quote.  */

void
pp_end_quote (pretty_printer *pp, bool show_color)
{
  pp_string (pp, colorize_stop (show_color));
  pp_string (pp, close_quote);
}


/* The string starting at P has LEN (at least 1) bytes left; if they
   start with a valid UTF-8 sequence, return the length of that
   sequence and set *VALUE to the value of that sequence, and
   otherwise return 0 and set *VALUE to (unsigned int) -1.  */

static int
decode_utf8_char (const unsigned char *p, size_t len, unsigned int *value)
{
  unsigned int t = *p;

  if (len == 0)
    abort ();
  if (t & 0x80)
    {
      size_t utf8_len = 0;
      unsigned int ch;
      size_t i;
      for (t = *p; t & 0x80; t <<= 1)
	utf8_len++;

      if (utf8_len > len || utf8_len < 2 || utf8_len > 6)
	{
	  *value = (unsigned int) -1;
	  return 0;
	}
      ch = *p & ((1 << (7 - utf8_len)) - 1);
      for (i = 1; i < utf8_len; i++)
	{
	  unsigned int u = p[i];
	  if ((u & 0xC0) != 0x80)
	    {
	      *value = (unsigned int) -1;
	      return 0;
	    }
	  ch = (ch << 6) | (u & 0x3F);
	}
      if (   (ch <=      0x7F && utf8_len > 1)
	  || (ch <=     0x7FF && utf8_len > 2)
	  || (ch <=    0xFFFF && utf8_len > 3)
	  || (ch <=  0x1FFFFF && utf8_len > 4)
	  || (ch <= 0x3FFFFFF && utf8_len > 5)
	  || (ch >= 0xD800 && ch <= 0xDFFF))
	{
	  *value = (unsigned int) -1;
	  return 0;
	}
      *value = ch;
      return utf8_len;
    }
  else
    {
      *value = t;
      return 1;
    }
}

/* Allocator for identifier_to_locale and corresponding function to
   free memory.  */

void *(*identifier_to_locale_alloc) (size_t) = xmalloc;
void (*identifier_to_locale_free) (void *) = free;

/* Given IDENT, an identifier in the internal encoding, return a
   version of IDENT suitable for diagnostics in the locale character
   set: either IDENT itself, or a string, allocated using
   identifier_to_locale_alloc, converted to the locale character set
   and using escape sequences if not representable in the locale
   character set or containing control characters or invalid byte
   sequences.  Existing backslashes in IDENT are not doubled, so the
   result may not uniquely specify the contents of an arbitrary byte
   sequence identifier.  */

const char *
identifier_to_locale (const char *ident)
{
  const unsigned char *uid = (const unsigned char *) ident;
  size_t idlen = strlen (ident);
  bool valid_printable_utf8 = true;
  bool all_ascii = true;
  size_t i;

  for (i = 0; i < idlen;)
    {
      unsigned int c;
      size_t utf8_len = decode_utf8_char (&uid[i], idlen - i, &c);
      if (utf8_len == 0 || c <= 0x1F || (c >= 0x7F && c <= 0x9F))
	{
	  valid_printable_utf8 = false;
	  break;
	}
      if (utf8_len > 1)
	all_ascii = false;
      i += utf8_len;
    }

  /* If IDENT contains invalid UTF-8 sequences (which may occur with
     attributes putting arbitrary byte sequences in identifiers), or
     control characters, we use octal escape sequences for all bytes
     outside printable ASCII.  */
  if (!valid_printable_utf8)
    {
      char *ret = (char *) identifier_to_locale_alloc (4 * idlen + 1);
      char *p = ret;
      for (i = 0; i < idlen; i++)
	{
	  if (uid[i] > 0x1F && uid[i] < 0x7F)
	    *p++ = uid[i];
	  else
	    {
	      sprintf (p, "\\%03o", uid[i]);
	      p += 4;
	    }
	}
      *p = 0;
      return ret;
    }

  /* Otherwise, if it is valid printable ASCII, or printable UTF-8
     with the locale character set being UTF-8, IDENT is used.  */
  if (all_ascii || locale_utf8)
    return ident;

  /* Otherwise IDENT is converted to the locale character set if
     possible.  */
#if defined ENABLE_NLS && defined HAVE_LANGINFO_CODESET && HAVE_ICONV
  if (locale_encoding != NULL)
    {
      iconv_t cd = iconv_open (locale_encoding, "UTF-8");
      bool conversion_ok = true;
      char *ret = NULL;
      if (cd != (iconv_t) -1)
	{
	  size_t ret_alloc = 4 * idlen + 1;
	  for (;;)
	    {
	      /* Repeat the whole conversion process as needed with
		 larger buffers so non-reversible transformations can
		 always be detected.  */
	      ICONV_CONST char *inbuf = CONST_CAST (char *, ident);
	      char *outbuf;
	      size_t inbytesleft = idlen;
	      size_t outbytesleft = ret_alloc - 1;
	      size_t iconv_ret;

	      ret = (char *) identifier_to_locale_alloc (ret_alloc);
	      outbuf = ret;

	      if (iconv (cd, 0, 0, 0, 0) == (size_t) -1)
		{
		  conversion_ok = false;
		  break;
		}

	      iconv_ret = iconv (cd, &inbuf, &inbytesleft,
				 &outbuf, &outbytesleft);
	      if (iconv_ret == (size_t) -1 || inbytesleft != 0)
		{
		  if (errno == E2BIG)
		    {
		      ret_alloc *= 2;
		      identifier_to_locale_free (ret);
		      ret = NULL;
		      continue;
		    }
		  else
		    {
		      conversion_ok = false;
		      break;
		    }
		}
	      else if (iconv_ret != 0)
		{
		  conversion_ok = false;
		  break;
		}
	      /* Return to initial shift state.  */
	      if (iconv (cd, 0, 0, &outbuf, &outbytesleft) == (size_t) -1)
		{
		  if (errno == E2BIG)
		    {
		      ret_alloc *= 2;
		      identifier_to_locale_free (ret);
		      ret = NULL;
		      continue;
		    }
		  else
		    {
		      conversion_ok = false;
		      break;
		    }
		}
	      *outbuf = 0;
	      break;
	    }
	  iconv_close (cd);
	  if (conversion_ok)
	    return ret;
	}
    }
#endif

  /* Otherwise, convert non-ASCII characters in IDENT to UCNs.  */
  {
    char *ret = (char *) identifier_to_locale_alloc (10 * idlen + 1);
    char *p = ret;
    for (i = 0; i < idlen;)
      {
	unsigned int c;
	size_t utf8_len = decode_utf8_char (&uid[i], idlen - i, &c);
	if (utf8_len == 1)
	  *p++ = uid[i];
	else
	  {
	    sprintf (p, "\\U%08x", c);
	    p += 10;
	  }
	i += utf8_len;
      }
    *p = 0;
    return ret;
  }
}

#if CHECKING_P

namespace selftest {

/* Smoketest for pretty_printer.  */

static void
test_basic_printing ()
{
  pretty_printer pp;
  pp_string (&pp, "hello");
  pp_space (&pp);
  pp_string (&pp, "world");

  ASSERT_STREQ ("hello world", pp_formatted_text (&pp));
}

/* Helper function for testing pp_format.
   Verify that pp_format (FMT, ...) followed by pp_output_formatted_text
   prints EXPECTED, assuming that pp_show_color is SHOW_COLOR.  */

static void
assert_pp_format_va (const location &loc, const char *expected,
		     bool show_color, const char *fmt, va_list *ap)
{
  pretty_printer pp;
  text_info ti;
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  ti.format_spec = fmt;
  ti.args_ptr = ap;
  ti.err_no = 0;
  ti.x_data = NULL;
  ti.m_richloc = &rich_loc;

  pp_show_color (&pp) = show_color;
  pp_format (&pp, &ti);
  pp_output_formatted_text (&pp);
  ASSERT_STREQ_AT (loc, expected, pp_formatted_text (&pp));
}

/* Verify that pp_format (FMT, ...) followed by pp_output_formatted_text
   prints EXPECTED, with show_color disabled.  */

static void
assert_pp_format (const location &loc, const char *expected,
		  const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  assert_pp_format_va (loc, expected, false, fmt, &ap);
  va_end (ap);
}

/* As above, but with colorization enabled.  */

static void
assert_pp_format_colored (const location &loc, const char *expected,
			  const char *fmt, ...)
{
  /* The tests of colorization assume the default color scheme.
     If GCC_COLORS is set, then the colors have potentially been
     overridden; skip the test.  */
  if (getenv ("GCC_COLORS"))
    return;

  va_list ap;

  va_start (ap, fmt);
  assert_pp_format_va (loc, expected, true, fmt, &ap);
  va_end (ap);
}

/* Helper function for calling testing pp_format,
   by calling assert_pp_format with various numbers of arguments.
   These exist mostly to avoid having to write SELFTEST_LOCATION
   throughout test_pp_format.  */

#define ASSERT_PP_FORMAT_1(EXPECTED, FMT, ARG1)		      \
  SELFTEST_BEGIN_STMT					      \
    assert_pp_format ((SELFTEST_LOCATION), (EXPECTED), (FMT), \
		      (ARG1));				      \
  SELFTEST_END_STMT

#define ASSERT_PP_FORMAT_2(EXPECTED, FMT, ARG1, ARG2)	      \
  SELFTEST_BEGIN_STMT					      \
    assert_pp_format ((SELFTEST_LOCATION), (EXPECTED), (FMT), \
		      (ARG1), (ARG2));			      \
  SELFTEST_END_STMT

#define ASSERT_PP_FORMAT_3(EXPECTED, FMT, ARG1, ARG2, ARG3)   \
  SELFTEST_BEGIN_STMT					      \
    assert_pp_format ((SELFTEST_LOCATION), (EXPECTED), (FMT), \
                      (ARG1), (ARG2), (ARG3));		      \
  SELFTEST_END_STMT

/* Verify that pp_format works, for various format codes.  */

static void
test_pp_format ()
{
  /* Avoid introducing locale-specific differences in the results
     by hardcoding open_quote and close_quote.  */
  auto_fix_quotes fix_quotes;

  /* Verify that plain text is passed through unchanged.  */
  assert_pp_format (SELFTEST_LOCATION, "unformatted", "unformatted");

  /* Verify various individual format codes, in the order listed in the
     comment for pp_format above.  For each code, we append a second
     argument with a known bit pattern (0x12345678), to ensure that we
     are consuming arguments correctly.  */
  ASSERT_PP_FORMAT_2 ("-27 12345678", "%d %x", -27, 0x12345678);
  ASSERT_PP_FORMAT_2 ("-5 12345678", "%i %x", -5, 0x12345678);
  ASSERT_PP_FORMAT_2 ("10 12345678", "%u %x", 10, 0x12345678);
  ASSERT_PP_FORMAT_2 ("17 12345678", "%o %x", 15, 0x12345678);
  ASSERT_PP_FORMAT_2 ("cafebabe 12345678", "%x %x", 0xcafebabe, 0x12345678);
  ASSERT_PP_FORMAT_2 ("-27 12345678", "%ld %x", (long)-27, 0x12345678);
  ASSERT_PP_FORMAT_2 ("-5 12345678", "%li %x", (long)-5, 0x12345678);
  ASSERT_PP_FORMAT_2 ("10 12345678", "%lu %x", (long)10, 0x12345678);
  ASSERT_PP_FORMAT_2 ("17 12345678", "%lo %x", (long)15, 0x12345678);
  ASSERT_PP_FORMAT_2 ("cafebabe 12345678", "%lx %x", (long)0xcafebabe,
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("-27 12345678", "%lld %x", (long long)-27, 0x12345678);
  ASSERT_PP_FORMAT_2 ("-5 12345678", "%lli %x", (long long)-5, 0x12345678);
  ASSERT_PP_FORMAT_2 ("10 12345678", "%llu %x", (long long)10, 0x12345678);
  ASSERT_PP_FORMAT_2 ("17 12345678", "%llo %x", (long long)15, 0x12345678);
  ASSERT_PP_FORMAT_2 ("cafebabe 12345678", "%llx %x", (long long)0xcafebabe,
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("-27 12345678", "%wd %x", (HOST_WIDE_INT)-27, 0x12345678);
  ASSERT_PP_FORMAT_2 ("-5 12345678", "%wi %x", (HOST_WIDE_INT)-5, 0x12345678);
  ASSERT_PP_FORMAT_2 ("10 12345678", "%wu %x", (unsigned HOST_WIDE_INT)10,
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("17 12345678", "%wo %x", (HOST_WIDE_INT)15, 0x12345678);
  ASSERT_PP_FORMAT_2 ("0xcafebabe 12345678", "%wx %x", (HOST_WIDE_INT)0xcafebabe,
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("A 12345678", "%c %x", 'A', 0x12345678);
  ASSERT_PP_FORMAT_2 ("hello world 12345678", "%s %x", "hello world",
		      0x12345678);

  /* Not nul-terminated.  */
  char arr[5] = { '1', '2', '3', '4', '5' };
  ASSERT_PP_FORMAT_3 ("123 12345678", "%.*s %x", 3, arr, 0x12345678);
  ASSERT_PP_FORMAT_3 ("1234 12345678", "%.*s %x", -1, "1234", 0x12345678);
  ASSERT_PP_FORMAT_3 ("12345 12345678", "%.*s %x", 7, "12345", 0x12345678);

  /* We can't test for %p; the pointer is printed in an implementation-defined
     manner.  */
  ASSERT_PP_FORMAT_2 ("normal colored normal 12345678",
		      "normal %rcolored%R normal %x",
		      "error", 0x12345678);
  assert_pp_format_colored
    (SELFTEST_LOCATION,
     "normal \33[01;31m\33[Kcolored\33[m\33[K normal 12345678",
     "normal %rcolored%R normal %x", "error", 0x12345678);
  /* TODO:
     %m: strerror(text->err_no) - does not consume a value from args_ptr.  */
  ASSERT_PP_FORMAT_1 ("% 12345678", "%% %x", 0x12345678);
  ASSERT_PP_FORMAT_1 ("` 12345678", "%< %x", 0x12345678);
  ASSERT_PP_FORMAT_1 ("' 12345678", "%> %x", 0x12345678);
  ASSERT_PP_FORMAT_1 ("' 12345678", "%' %x", 0x12345678);
  ASSERT_PP_FORMAT_3 ("abc 12345678", "%.*s %x", 3, "abcdef", 0x12345678);
  ASSERT_PP_FORMAT_2 ("abc 12345678", "%.3s %x", "abcdef", 0x12345678);

  /* Verify flag 'q'.  */
  ASSERT_PP_FORMAT_2 ("`foo' 12345678", "%qs %x", "foo", 0x12345678);
  assert_pp_format_colored (SELFTEST_LOCATION,
			    "`\33[01m\33[Kfoo\33[m\33[K' 12345678", "%qs %x",
			    "foo", 0x12345678);

  /* Verify %Z.  */
  int v[] = { 1, 2, 3 }; 
  ASSERT_PP_FORMAT_3 ("1, 2, 3 12345678", "%Z %x", v, 3, 0x12345678);

  int v2[] = { 0 }; 
  ASSERT_PP_FORMAT_3 ("0 12345678", "%Z %x", v2, 1, 0x12345678);

  /* Verify that combinations work, along with unformatted text.  */
  assert_pp_format (SELFTEST_LOCATION,
		    "the quick brown fox jumps over the lazy dog",
		    "the %s %s %s jumps over the %s %s",
		    "quick", "brown", "fox", "lazy", "dog");
  assert_pp_format (SELFTEST_LOCATION, "item 3 of 7", "item %i of %i", 3, 7);
  assert_pp_format (SELFTEST_LOCATION, "problem with `bar' at line 10",
		    "problem with %qs at line %i", "bar", 10);
}

/* Run all of the selftests within this file.  */

void
pretty_print_c_tests ()
{
  test_basic_printing ();
  test_pp_format ();
}

} // namespace selftest

#endif /* CHECKING_P */
