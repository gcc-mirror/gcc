/* Various declarations for language-independent pretty-print subroutines.
   Copyright (C) 2003-2024 Free Software Foundation, Inc.
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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include "pretty-print.h"
#include "pretty-print-urlifier.h"
#include "diagnostic-color.h"
#include "diagnostic-event-id.h"
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
      if (attrib_add & COMMON_LVB_REVERSE_VIDEO)
	{
	  /* COMMON_LVB_REVERSE_VIDEO is only effective for DBCS.
	   * Swap foreground and background colors by hand.
	   */
	  attrib_add = (attrib_add & 0xFF00)
			| ((attrib_add & 0x00F0) >> 4)
			| ((attrib_add & 0x000F) << 4);
	  attrib_add &= ~COMMON_LVB_REVERSE_VIDEO;
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

  return 1;
}

#endif /* __MINGW32__ */

static int
decode_utf8_char (const unsigned char *, size_t len, unsigned int *);
static void pp_quoted_string (pretty_printer *, const char *, size_t = -1);

/* Overwrite the given location/range within this text_info's rich_location.
   For use e.g. when implementing "+" in client format decoders.  */

void
text_info::set_location (unsigned int idx, location_t loc,
			 enum range_display_kind range_display_kind)
{
  gcc_checking_assert (m_richloc);
  m_richloc->set_range (idx, loc, range_display_kind);
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

#ifndef PTRDIFF_MAX
#define PTRDIFF_MAX INTTYPE_MAXIMUM (ptrdiff_t)
#endif

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
        pp_scalar (PP, "%" HOST_LONG_LONG_FORMAT F,          \
                   va_arg (ARG, long long T));               \
        break;                                               \
                                                             \
      case 3:                                                \
        if (T (-1) < T (0))                                  \
          pp_scalar (PP, "%" GCC_PRISZ F,                    \
                     (fmt_size_t) va_arg (ARG, ssize_t));    \
        else                                                 \
          pp_scalar (PP, "%" GCC_PRISZ F,                    \
                     (fmt_size_t) va_arg (ARG, size_t));     \
        break;                                               \
                                                             \
      case 4:                                                \
        if (T (-1) >= T (0))                                 \
          {                                                  \
            unsigned long long a = va_arg (ARG, ptrdiff_t);  \
            unsigned long long m = PTRDIFF_MAX;              \
            m = 2 * m + 1;                                   \
            pp_scalar (PP, "%" HOST_LONG_LONG_FORMAT F,      \
		       a & m);                               \
          }                                                  \
        else if (sizeof (ptrdiff_t) <= sizeof (int))         \
          pp_scalar (PP, "%" F,                              \
                     (int) va_arg (ARG, ptrdiff_t));         \
        else if (sizeof (ptrdiff_t) <= sizeof (long))        \
          pp_scalar (PP, "%l" F,                             \
                     (long int) va_arg (ARG, ptrdiff_t));    \
        else                                                 \
          pp_scalar (PP, "%" HOST_LONG_LONG_FORMAT F,        \
                     (long long int)                         \
                     va_arg (ARG, ptrdiff_t));               \
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
pp_wide_integer (pretty_printer *pp, const poly_int<N, T> &x)
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

template void pp_wide_integer (pretty_printer *, const poly_uint16 &);
template void pp_wide_integer (pretty_printer *, const poly_int64 &);
template void pp_wide_integer (pretty_printer *, const poly_uint64 &);

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

/* As pp_write_text_to_stream, but for GraphViz HTML-like strings.

   Flush the formatted text of pretty-printer PP onto the attached stream,
   escaping these characters
     " & < >
   using XML escape sequences.

   http://www.graphviz.org/doc/info/lang.html#html states:
      special XML escape sequences for ", &, <, and > may be necessary in
      order to embed these characters in attribute values or raw text
   This doesn't list "'" (which would normally be escaped in XML
   as "&apos;" or in HTML as "&#39;");.

   Experiments show that escaping "'" doesn't seem to be necessary.  */

void
pp_write_text_as_html_like_dot_to_stream (pretty_printer *pp)
{
  const char *text = pp_formatted_text (pp);
  const char *p = text;
  FILE *fp = pp_buffer (pp)->stream;

  for (;*p; p++)
    {
      switch (*p)
	{
	case '"':
	  fputs ("&quot;", fp);
	  break;
	case '&':
	  fputs ("&amp;", fp);
	  break;
	case '<':
	  fputs ("&lt;", fp);
	  break;
	case '>':
	  fputs ("&gt;",fp);
	  break;

	default:
	  fputc (*p, fp);
	  break;
	}
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

static const char *get_end_url_string (pretty_printer *);

/* Append STR to OSTACK, without a null-terminator.  */

static void
obstack_append_string (obstack *ostack, const char *str)
{
  obstack_grow (ostack, str, strlen (str));
}

/* Append STR to OSTACK, without a null-terminator.  */

static void
obstack_append_string (obstack *ostack, const char *str, size_t len)
{
  obstack_grow (ostack, str, len);
}

/* Given quoted text within the buffer OBSTACK
   at the half-open interval [QUOTED_TEXT_START_IDX, QUOTED_TEXT_END_IDX),
   potentially use URLIFIER (if non-null) to see if there's a URL for the
   quoted text.

   If so, replace the quoted part of the text in the buffer with a URLified
   version of the text, using PP's settings.

   For example, given this is the buffer:
     "this is a test `hello worldTRAILING-CONTENT"
     .................^~~~~~~~~~~
   with the quoted text starting at the 'h' of "hello world", the buffer
   becomes:
     "this is a test `BEGIN_URL(URL)hello worldEND(URL)TRAILING-CONTENT"
     .................^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     .................-----------replacement-----------

   Return the new offset into the buffer of the quoted text endpoint i.e.
   the offset of "TRAILING-CONTENT" in the above.  */

static size_t
urlify_quoted_string (pretty_printer *pp,
		      obstack *obstack,
		      const urlifier *urlifier,
		      size_t quoted_text_start_idx,
		      size_t quoted_text_end_idx)
{
  if (pp->url_format == URL_FORMAT_NONE)
    return quoted_text_end_idx;
  if (!urlifier)
    return quoted_text_end_idx;

  const size_t quoted_len = quoted_text_end_idx - quoted_text_start_idx;
  if (quoted_len == 0)
    /* Empty quoted string; do nothing.  */
    return quoted_text_end_idx;
  const char *start = (obstack->object_base + quoted_text_start_idx);
  char *url = urlifier->get_url_for_quoted_text (start, quoted_len);
  if (!url)
    /* No URL for this quoted text; do nothing.  */
    return quoted_text_end_idx;

  /* Stash a copy of the remainder of the chunk.  */
  char *text = xstrndup (start,
			 obstack_object_size (obstack) - quoted_text_start_idx);

  /* Replace quoted text...  */
  obstack->next_free = obstack->object_base + quoted_text_start_idx;

  /*  ...with URLified version of the text.  */
  /* Begin URL.  */
  switch (pp->url_format)
    {
    default:
    case URL_FORMAT_NONE:
      gcc_unreachable ();
    case URL_FORMAT_ST:
      obstack_append_string (obstack, "\33]8;;");
      obstack_append_string (obstack, url);
      obstack_append_string (obstack, "\33\\");
      break;
    case URL_FORMAT_BEL:
      obstack_append_string (obstack, "\33]8;;");
      obstack_append_string (obstack, url);
      obstack_append_string (obstack, "\a");
      break;
    }
  /* Add back the quoted part of the text.  */
  obstack_append_string (obstack, text, quoted_len);
  /* End URL.  */
  obstack_append_string (obstack,
			 get_end_url_string (pp));

  size_t new_end_idx = obstack_object_size (obstack);

  /* Add back the remainder of the text after the quoted part.  */
  obstack_append_string (obstack, text + quoted_len);
  free (text);
  free (url);
  return new_end_idx;
}

/* A class for tracking quoted text within a buffer for
   use by a urlifier.  */

class quoting_info
{
public:
  /* Called when quoted text is begun in phase 1 or 2.  */
  void on_begin_quote (const output_buffer &buf,
		       unsigned chunk_idx)
  {
    /* Stash location of start of quoted string.  */
    size_t byte_offset = obstack_object_size (&buf.chunk_obstack);
    m_loc_last_open_quote = location (chunk_idx, byte_offset);
  }

  /* Called when quoted text is ended in phase 1 or 2.  */
  void on_end_quote (pretty_printer *pp,
		     output_buffer &buf,
		     unsigned chunk_idx,
		     const urlifier &urlifier)
  {
    /* If possible, do urlification now.  */
    if (chunk_idx == m_loc_last_open_quote.m_chunk_idx)
      {
	urlify_quoted_string (pp,
			      &buf.chunk_obstack,
			      &urlifier,
			      m_loc_last_open_quote.m_byte_offset,
			      obstack_object_size (&buf.chunk_obstack));
	m_loc_last_open_quote = location ();
	return;
      }
    /* Otherwise the quoted text straddles multiple chunks.
       Stash the location of end of quoted string for use in phase 3.  */
    size_t byte_offset = obstack_object_size (&buf.chunk_obstack);
    m_phase_3_quotes.push_back (run (m_loc_last_open_quote,
				     location (chunk_idx, byte_offset)));
    m_loc_last_open_quote = location ();
  }

  bool has_phase_3_quotes_p () const
  {
    return m_phase_3_quotes.size () > 0;
  }
  void handle_phase_3 (pretty_printer *pp,
		       const urlifier &urlifier);

private:
  struct location
  {
    location ()
    : m_chunk_idx (UINT_MAX),
      m_byte_offset (SIZE_MAX)
    {
    }

    location (unsigned chunk_idx,
	      size_t byte_offset)
    : m_chunk_idx (chunk_idx),
      m_byte_offset (byte_offset)
    {
    }

    unsigned m_chunk_idx;
    size_t m_byte_offset;
  };

  struct run
  {
    run (location start, location end)
    : m_start (start), m_end (end)
    {
    }

    location m_start;
    location m_end;
  };

  location m_loc_last_open_quote;
  std::vector<run> m_phase_3_quotes;
};

static void
on_begin_quote (const output_buffer &buf,
		unsigned chunk_idx,
		const urlifier *urlifier)
{
  if (!urlifier)
    return;
  if (!buf.cur_chunk_array->m_quotes)
    buf.cur_chunk_array->m_quotes = new quoting_info ();
  buf.cur_chunk_array->m_quotes->on_begin_quote (buf, chunk_idx);
}

static void
on_end_quote (pretty_printer *pp,
	      output_buffer &buf,
	      unsigned chunk_idx,
	      const urlifier *urlifier)
{
  if (!urlifier)
    return;
  if (!buf.cur_chunk_array->m_quotes)
    buf.cur_chunk_array->m_quotes = new quoting_info ();
  buf.cur_chunk_array->m_quotes->on_end_quote (pp, buf, chunk_idx, *urlifier);
}

/* The following format specifiers are recognized as being client independent:
   %d, %i: (signed) integer in base ten.
   %u: unsigned integer in base ten.
   %o: unsigned integer in base eight.
   %x: unsigned integer in base sixteen.
   %ld, %li, %lo, %lu, %lx: long versions of the above.
   %lld, %lli, %llo, %llu, %llx: long long versions.
   %wd, %wi, %wo, %wu, %wx: HOST_WIDE_INT versions.
   %zd, %zi, %zo, %zu, %zx: size_t versions.
   %td, %ti, %to, %tu, %tx: ptrdiff_t versions.
   %f: double
   %c: character.
   %s: string.
   %p: pointer (printed in a host-dependent manner).
   %r: if pp_show_color(pp), switch to color identified by const char *.
   %R: if pp_show_color(pp), reset color.
   %m: strerror(text->err_no) - does not consume a value from args_ptr.
   %%: '%'.
   %<: opening quote.
   %>: closing quote.
   %{: URL start.  Consumes a const char * argument for the URL.
   %}: URL end.    Does not consume any arguments.
   %': apostrophe (should only be used in untranslated messages;
       translations should use appropriate punctuation directly).
   %@: diagnostic_event_id_ptr, for which event_id->known_p () must be true.
   %.*s: a substring the length of which is specified by an argument
	 integer.
   %Ns: likewise, but length specified as constant in the format string.
   Flag 'q': quote formatted text (must come immediately after '%').
   %Z: Requires two arguments - array of int, and len. Prints elements
   of the array.

   Arguments can be used sequentially, or through %N$ resp. *N$
   notation Nth argument after the format string.  If %N$ / *N$
   notation is used, it must be used for all arguments, except %m, %%,
   %<, %>, %} and %', which may not have a number, as they do not consume
   an argument.  When %M$.*N$s is used, M must be N + 1.  (This may
   also be written %M$.*s, provided N is not otherwise used.)  The
   format string must have conversion specifiers with argument numbers
   1 up to highest argument; each argument may only be used once.
   A format string can have at most 30 arguments.  */

/* Formatting phases 1 and 2: render TEXT->format_spec plus
   text->m_args_ptr into a series of chunks in pp_buffer (PP)->args[].
   Phase 3 is in pp_output_formatted_text.

   If URLIFIER is non-NULL, then use it to add URLs for quoted
   strings, so that e.g.
     "before %<quoted%> after"
   with a URLIFIER that has a URL for "quoted" might be emitted as:
     "before `BEGIN_URL(http://example.com)quotedEND_URL' after"
   This is handled here for message fragments that are:
   - quoted entirely in phase 1 (e.g. "%<this is quoted%>"), or
   - quoted entirely in phase 2 (e.g. "%qs"),
   Quoted fragments that use a mixture of both phases
   (e.g. "%<this is a mixture: %s %>")
   are stashed into the output_buffer's m_quotes for use in phase 3.  */

void
pp_format (pretty_printer *pp,
	   text_info *text,
	   const urlifier *urlifier)
{
  output_buffer * const buffer = pp_buffer (pp);
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
  new_chunk_array->m_quotes = nullptr;
  buffer->cur_chunk_array = new_chunk_array;
  args = new_chunk_array->args;

  /* Formatting phase 1: split up TEXT->format_spec into chunks in
     pp_buffer (PP)->args[].  Even-numbered chunks are to be output
     verbatim, odd-numbered chunks are format specifiers.
     %m, %%, %<, %>, %} and %' are replaced with the appropriate text at
     this point.  */

  memset (formatters, 0, sizeof formatters);

  for (p = text->m_format_spec; *p; )
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

	    on_begin_quote (*buffer, chunk, urlifier);
	    continue;
	  }

	case '>':
	  {
	    on_end_quote (pp, *buffer, chunk, urlifier);

	    const char *colorstr = colorize_stop (pp_show_color (pp));
	    obstack_grow (&buffer->chunk_obstack, colorstr, strlen (colorstr));
	  }
	  /* FALLTHRU */
	case '\'':
	  obstack_grow (&buffer->chunk_obstack,
			close_quote, strlen (close_quote));
	  p++;
	  continue;

	case '}':
	  {
	    const char *endurlstr = get_end_url_string (pp);
	    obstack_grow (&buffer->chunk_obstack, endurlstr,
			  strlen (endurlstr));
	  }
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
	    const char *errstr = xstrerror (text->m_err_no);
	    obstack_grow (&buffer->chunk_obstack, errstr, strlen (errstr));
	  }
	  p++;
	  continue;

	default:
	  /* Handled in phase 2.  Terminate the plain chunk here.  */
	  obstack_1grow (&buffer->chunk_obstack, '\0');
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
      while (strchr ("qwlzt+#", p[-1]));

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
  const int old_line_length = buffer->line_length;
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

	    case 'z':
	      gcc_assert (!precision);
	      precision = 3;
	      continue;

	    case 't':
	      gcc_assert (!precision);
	      precision = 4;
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
	{
	  pp_begin_quote (pp, pp_show_color (pp));
	  on_begin_quote (*buffer, chunk, urlifier);
	}

      switch (*p)
	{
	case 'r':
	  pp_string (pp, colorize_start (pp_show_color (pp),
					 va_arg (*text->m_args_ptr,
						 const char *)));
	  break;

	case 'c':
	  {
	    /* When quoting, print alphanumeric, punctuation, and the space
	       character unchanged, and all others in hexadecimal with the
	       "\x" prefix.  Otherwise print them all unchanged.  */
	    int chr = va_arg (*text->m_args_ptr, int);
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
	    pp_wide_integer (pp, va_arg (*text->m_args_ptr, HOST_WIDE_INT));
	  else
	    pp_integer_with_precision (pp, *text->m_args_ptr, precision,
				       int, "d");
	  break;

	case 'o':
	  if (wide)
	    pp_scalar (pp, "%" HOST_WIDE_INT_PRINT "o",
		       va_arg (*text->m_args_ptr, unsigned HOST_WIDE_INT));
	  else
	    pp_integer_with_precision (pp, *text->m_args_ptr, precision,
				       unsigned, "o");
	  break;

	case 's':
	  if (quote)
	    pp_quoted_string (pp, va_arg (*text->m_args_ptr, const char *));
	  else
	    pp_string (pp, va_arg (*text->m_args_ptr, const char *));
	  break;

	case 'p':
	  pp_pointer (pp, va_arg (*text->m_args_ptr, void *));
	  break;

	case 'u':
	  if (wide)
	    pp_scalar (pp, HOST_WIDE_INT_PRINT_UNSIGNED,
		       va_arg (*text->m_args_ptr, unsigned HOST_WIDE_INT));
	  else
	    pp_integer_with_precision (pp, *text->m_args_ptr, precision,
				       unsigned, "u");
	  break;

	case 'f':
	  pp_double (pp, va_arg (*text->m_args_ptr, double));
	  break;

	case 'Z':
	  {
	    int *v = va_arg (*text->m_args_ptr, int *);
	    unsigned len = va_arg (*text->m_args_ptr, unsigned);

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
		       va_arg (*text->m_args_ptr, unsigned HOST_WIDE_INT));
	  else
	    pp_integer_with_precision (pp, *text->m_args_ptr, precision,
				       unsigned, "x");
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
		n = va_arg (*text->m_args_ptr, int);

		/* This consumes a second entry in the formatters array.  */
		gcc_assert (formatters[argno] == formatters[argno+1]);
		argno++;
	      }

	    s = va_arg (*text->m_args_ptr, const char *);

	    /* Append the lesser of precision and strlen (s) characters
	       from the array (which need not be a nul-terminated string).
	       Negative precision is treated as if it were omitted.  */
	    size_t len = n < 0 ? strlen (s) : strnlen (s, n);

	    pp_append_text (pp, s, s + len);
	  }
	  break;

	case '@':
	  {
	    /* diagnostic_event_id_t *.  */
	    diagnostic_event_id_ptr event_id
	      = va_arg (*text->m_args_ptr, diagnostic_event_id_ptr);
	    gcc_assert (event_id->known_p ());

	    pp_string (pp, colorize_start (pp_show_color (pp), "path"));
	    pp_character (pp, '(');
	    pp_decimal_int (pp, event_id->one_based ());
	    pp_character (pp, ')');
	    pp_string (pp, colorize_stop (pp_show_color (pp)));
	  }
	  break;

	case '{':
	  pp_begin_url (pp, va_arg (*text->m_args_ptr, const char *));
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
	{
	  on_end_quote (pp, *buffer, chunk, urlifier);
	  pp_end_quote (pp, pp_show_color (pp));
	}

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
  buffer->line_length = old_line_length;
  pp_wrapping_mode (pp) = old_wrapping_mode;
  pp_clear_state (pp);
}

struct auto_obstack
{
  auto_obstack ()
  {
    obstack_init (&m_obstack);
  }

  ~auto_obstack ()
  {
    obstack_free (&m_obstack, NULL);
  }

  void grow (const void *src, size_t length)
  {
    obstack_grow (&m_obstack, src, length);
  }

  void *object_base () const
  {
    return m_obstack.object_base;
  }

  size_t object_size () const
  {
    return obstack_object_size (&m_obstack);
  }

  obstack m_obstack;
};

/* Subroutine of pp_output_formatted_text for the awkward case where
   quoted text straddles multiple chunks.

   Flush PP's buffer's chunks to PP's output buffer, whilst inserting
   URLs for any quoted text that should be URLified.

   For example, given:
   |  pp_format (pp,
   |            "unrecognized option %qs; did you mean %<-%s%>",
   |            "foo", "foption");
   we would have these chunks:
   |  chunk 0: "unrecognized option "
   |  chunk 1: "`foo'" (already checked for urlification)
   |  chunk 2: "; did you mean `-"
   |                           ^*
   |  chunk 3: "foption"
   |            *******
   |  chunk 4: "'"
   |            ^
   and this quoting_info would have recorded the open quote near the end
   of chunk 2 and close quote at the start of chunk 4; this function would
   check the combination of the end of chunk 2 and all of chunk 3 ("-foption")
   for urlification.  */

void
quoting_info::handle_phase_3 (pretty_printer *pp,
			      const urlifier &urlifier)
{
  unsigned int chunk;
  output_buffer * const buffer = pp_buffer (pp);
  struct chunk_info *chunk_array = buffer->cur_chunk_array;
  const char **args = chunk_array->args;

  /* We need to construct the string into an intermediate buffer
     for this case, since using pp_string can introduce prefixes
     and line-wrapping, and omit whitespace at the start of lines.  */
  auto_obstack combined_buf;

  /* Iterate simultaneously through both
     - the chunks and
     - the runs of quoted characters
     Accumulate text from the chunks into combined_buf, and handle
     runs of quoted characters when handling the chunks they
     correspond to.  */
  size_t start_of_run_byte_offset = 0;
  std::vector<quoting_info::run>::const_iterator iter_run
    = buffer->cur_chunk_array->m_quotes->m_phase_3_quotes.begin ();
  std::vector<quoting_info::run>::const_iterator end_runs
    = buffer->cur_chunk_array->m_quotes->m_phase_3_quotes.end ();
  for (chunk = 0; args[chunk]; chunk++)
    {
      size_t start_of_chunk_idx = combined_buf.object_size ();

      combined_buf.grow (args[chunk], strlen (args[chunk]));

      if (iter_run != end_runs
	  && chunk == iter_run->m_end.m_chunk_idx)
	{
	  /* A run is ending; consider for it urlification.  */
	  const size_t end_of_run_byte_offset
	    = start_of_chunk_idx + iter_run->m_end.m_byte_offset;
	  const size_t end_offset
	    = urlify_quoted_string (pp,
				    &combined_buf.m_obstack,
				    &urlifier,
				    start_of_run_byte_offset,
				    end_of_run_byte_offset);

	  /* If URLification occurred it will have grown the buffer.
	     We need to update start_of_chunk_idx so that offsets
	     relative to it are still correct, for the case where
	     we have a chunk that both ends a quoted run and starts
	     another quoted run.  */
	  gcc_assert (end_offset >= end_of_run_byte_offset);
	  start_of_chunk_idx += end_offset - end_of_run_byte_offset;

	  iter_run++;
	}
      if (iter_run != end_runs
	  && chunk == iter_run->m_start.m_chunk_idx)
	{
	  /* Note where the run starts w.r.t. the composed buffer.  */
	  start_of_run_byte_offset
	    = start_of_chunk_idx + iter_run->m_start.m_byte_offset;
	}
    }

  /* Now print to PP.  */
  const char *start
    = static_cast <const char *> (combined_buf.object_base ());
  pp_maybe_wrap_text (pp, start, start + combined_buf.object_size ());
}

/* Format of a message pointed to by TEXT.
   If URLIFIER is non-null then use it on any quoted text that was not
   handled in phases 1 or 2 to potentially add URLs.  */

void
pp_output_formatted_text (pretty_printer *pp,
			  const urlifier *urlifier)
{
  unsigned int chunk;
  output_buffer * const buffer = pp_buffer (pp);
  struct chunk_info *chunk_array = buffer->cur_chunk_array;
  const char **args = chunk_array->args;

  gcc_assert (buffer->obstack == &buffer->formatted_obstack);

  /* This is a third phase, first 2 phases done in pp_format_args.
     Now we actually print it.  */

  /* If we have any deferred urlification, handle it now.  */
  if (urlifier
      && pp->url_format != URL_FORMAT_NONE
      && buffer->cur_chunk_array->m_quotes
      && buffer->cur_chunk_array->m_quotes->has_phase_3_quotes_p ())
    buffer->cur_chunk_array->m_quotes->handle_phase_3 (pp, *urlifier);
  else
    for (chunk = 0; args[chunk]; chunk++)
      pp_string (pp, args[chunk]);

  /* Deallocate the chunk structure and everything after it (i.e. the
     associated series of formatted strings).  */
  delete buffer->cur_chunk_array->m_quotes;
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

/* Set PREFIX for PRETTY-PRINTER, taking ownership of PREFIX, which
   will eventually be free-ed.  */

void
pp_set_prefix (pretty_printer *pp, char *prefix)
{
  free (pp->prefix);
  pp->prefix = prefix;
  pp_set_real_maximum_length (pp);
  pp->emitted_prefix = false;
  pp_indentation (pp) = 0;
}

/* Take ownership of PP's prefix, setting it to NULL.
   This allows clients to save, override, and then restore an existing
   prefix, without it being free-ed.  */

char *
pp_take_prefix (pretty_printer *pp)
{
  char *result = pp->prefix;
  pp->prefix = NULL;
  return result;
}

/* Free PRETTY-PRINTER's prefix, a previously malloc()'d string.  */
void
pp_destroy_prefix (pretty_printer *pp)
{
  if (pp->prefix != NULL)
    {
      free (pp->prefix);
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

/* Construct a PRETTY-PRINTER of MAXIMUM_LENGTH characters per line.  */

pretty_printer::pretty_printer (int maximum_length)
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
    show_color (),
    url_format (URL_FORMAT_NONE),
    m_skipping_null_url (false)
{
  pp_line_cutoff (this) = maximum_length;
  /* By default, we emit prefixes once per message.  */
  pp_prefixing_rule (this) = DIAGNOSTICS_SHOW_PREFIX_ONCE;
  pp_set_prefix (this, NULL);
}

/* Copy constructor for pretty_printer.  */

pretty_printer::pretty_printer (const pretty_printer &other)
: buffer (new (XCNEW (output_buffer)) output_buffer ()),
  prefix (),
  padding (other.padding),
  maximum_length (other.maximum_length),
  indent_skip (other.indent_skip),
  wrapping (other.wrapping),
  format_decoder (other.format_decoder),
  m_format_postprocessor (NULL),
  emitted_prefix (other.emitted_prefix),
  need_newline (other.need_newline),
  translate_identifiers (other.translate_identifiers),
  show_color (other.show_color),
  url_format (other.url_format),
  m_skipping_null_url (false)
{
  pp_line_cutoff (this) = maximum_length;
  /* By default, we emit prefixes once per message.  */
  pp_prefixing_rule (this) = pp_prefixing_rule (&other);
  pp_set_prefix (this, NULL);

  if (other.m_format_postprocessor)
    m_format_postprocessor = other.m_format_postprocessor->clone ();
}

pretty_printer::~pretty_printer ()
{
  if (m_format_postprocessor)
    delete m_format_postprocessor;
  buffer->~output_buffer ();
  XDELETE (buffer);
  free (prefix);
}

/* Base class implementation of pretty_printer::clone vfunc.  */

pretty_printer *
pretty_printer::clone () const
{
  return new pretty_printer (*this);
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
  va_list ap;

  va_start (ap, msg);
  text_info text (msg, &ap, errno);
  pp_format (pp, &text);
  pp_output_formatted_text (pp);
  va_end (ap);
}


/* Output MESSAGE verbatim into BUFFER.  */
void
pp_verbatim (pretty_printer *pp, const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  text_info text (msg, &ap, errno);
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
      /* If printing UTF-8, don't wrap in the middle of a sequence.  */
      && (((unsigned int) c) & 0xC0) != 0x80
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

/* Append code point C to the output area of PRETTY-PRINTER, encoding it
   as UTF-8.  */

void
pp_unicode_character (pretty_printer *pp, unsigned c)
{
  static const uchar masks[6] =  { 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };
  static const uchar limits[6] = { 0x80, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE };
  size_t nbytes;
  uchar buf[6], *p = &buf[6];

  nbytes = 1;
  if (c < 0x80)
    *--p = c;
  else
    {
      do
	{
	  *--p = ((c & 0x3F) | 0x80);
	  c >>= 6;
	  nbytes++;
	}
      while (c >= 0x3F || (c & limits[nbytes-1]));
      *--p = (c | masks[nbytes-1]);
    }

  pp_append_r (pp, (const char *)p, nbytes);
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

      /* Don't escape a valid UTF-8 extended char.  */
      const unsigned char *ups = (const unsigned char *) ps;
      if (*ups & 0x80)
	{
	  unsigned int extended_char;
	  const int valid_utf8_len = decode_utf8_char (ups, n, &extended_char);
	  if (valid_utf8_len > 0)
	    {
	      ps += valid_utf8_len - 1;
	      n -= valid_utf8_len - 1;
	      continue;
	    }
	}

      if (last < ps)
	pp_maybe_wrap_text (pp, last, ps);

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

/* Support for encoding URLs.
   See egmontkob/Hyperlinks_in_Terminal_Emulators.md
   ( https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda ).

   > A hyperlink is opened upon encountering an OSC 8 escape sequence with
   > the target URI. The syntax is
   >
   >  OSC 8 ; params ; URI ST
   >
   > A hyperlink is closed with the same escape sequence, omitting the
   > parameters and the URI but keeping the separators:
   >
   > OSC 8 ; ; ST
   >
   > OSC (operating system command) is typically ESC ].

   Use BEL instead of ST, as that is currently rendered better in some
   terminal emulators that don't support OSC 8, like konsole.  */

/* If URL-printing is enabled, write an "open URL" escape sequence to PP
   for the given URL.  */

void
pp_begin_url (pretty_printer *pp, const char *url)
{
  if (!url)
    {
      /* Handle null URL by skipping all output here,
	 and in the next pp_end_url.  */
      pp->m_skipping_null_url = true;
      return;
    }
  switch (pp->url_format)
    {
    case URL_FORMAT_NONE:
      break;
    case URL_FORMAT_ST:
      pp_string (pp, "\33]8;;");
      pp_string (pp, url);
      pp_string (pp, "\33\\");
      break;
    case URL_FORMAT_BEL:
      pp_string (pp, "\33]8;;");
      pp_string (pp, url);
      pp_string (pp, "\a");
      break;
    default:
      gcc_unreachable ();
    }
}

/* Helper function for pp_end_url and pp_format, return the "close URL" escape
   sequence string.  */

static const char *
get_end_url_string (pretty_printer *pp)
{
  switch (pp->url_format)
    {
    case URL_FORMAT_NONE:
      return "";
    case URL_FORMAT_ST:
      return "\33]8;;\33\\";
    case URL_FORMAT_BEL:
      return "\33]8;;\a";
    default:
      gcc_unreachable ();
    }
}

/* If URL-printing is enabled, write a "close URL" escape sequence to PP.  */

void
pp_end_url (pretty_printer *pp)
{
  if (pp->m_skipping_null_url)
    {
      /* We gracefully handle pp_begin_url (NULL) by omitting output for
	 both begin and end.  Here we handle the latter.  */
      pp->m_skipping_null_url = false;
      return;
    }
  if (pp->url_format != URL_FORMAT_NONE)
    pp_string (pp, get_end_url_string (pp));
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
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  text_info ti (fmt, ap, 0, nullptr, &rich_loc);

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
  ASSERT_PP_FORMAT_2 ("-27 12345678", "%wd %x", HOST_WIDE_INT_C (-27),
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("-5 12345678", "%wi %x", HOST_WIDE_INT_C (-5),
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("10 12345678", "%wu %x", HOST_WIDE_INT_UC (10),
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("17 12345678", "%wo %x", HOST_WIDE_INT_C (15),
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("0xcafebabe 12345678", "%wx %x",
		      HOST_WIDE_INT_C (0xcafebabe), 0x12345678);
  ASSERT_PP_FORMAT_2 ("-27 12345678", "%zd %x", (ssize_t)-27, 0x12345678);
  ASSERT_PP_FORMAT_2 ("-5 12345678", "%zi %x", (ssize_t)-5, 0x12345678);
  ASSERT_PP_FORMAT_2 ("10 12345678", "%zu %x", (size_t)10, 0x12345678);
  ASSERT_PP_FORMAT_2 ("17 12345678", "%zo %x", (size_t)15, 0x12345678);
  ASSERT_PP_FORMAT_2 ("cafebabe 12345678", "%zx %x", (size_t)0xcafebabe,
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("-27 12345678", "%td %x", (ptrdiff_t)-27, 0x12345678);
  ASSERT_PP_FORMAT_2 ("-5 12345678", "%ti %x", (ptrdiff_t)-5, 0x12345678);
  ASSERT_PP_FORMAT_2 ("10 12345678", "%tu %x", (ptrdiff_t)10, 0x12345678);
  ASSERT_PP_FORMAT_2 ("17 12345678", "%to %x", (ptrdiff_t)15, 0x12345678);
  ASSERT_PP_FORMAT_2 ("1afebabe 12345678", "%tx %x", (ptrdiff_t)0x1afebabe,
		      0x12345678);
  ASSERT_PP_FORMAT_2 ("1.000000 12345678", "%f %x", 1.0, 0x12345678);
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
  /* Verify "%@".  */
  {
    diagnostic_event_id_t first (2);
    diagnostic_event_id_t second (7);

    ASSERT_PP_FORMAT_2 ("first `free' at (3); second `free' at (8)",
			"first %<free%> at %@; second %<free%> at %@",
			&first, &second);
    assert_pp_format_colored
      (SELFTEST_LOCATION,
       "first `[01m[Kfree[m[K' at [01;36m[K(3)[m[K;"
       " second `[01m[Kfree[m[K' at [01;36m[K(8)[m[K",
       "first %<free%> at %@; second %<free%> at %@",
       &first, &second);
  }

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

  /* Verified numbered args.  */
  assert_pp_format (SELFTEST_LOCATION,
		    "foo: second bar: first",
		    "foo: %2$s bar: %1$s",
		    "first", "second");
  assert_pp_format (SELFTEST_LOCATION,
		    "foo: 1066 bar: 1776",
		    "foo: %2$i bar: %1$i",
		    1776, 1066);
  assert_pp_format (SELFTEST_LOCATION,
		    "foo: second bar: 1776",
		    "foo: %2$s bar: %1$i",
		    1776, "second");
}

/* A subclass of pretty_printer for use by test_prefixes_and_wrapping.  */

class test_pretty_printer : public pretty_printer
{
 public:
  test_pretty_printer (enum diagnostic_prefixing_rule_t rule,
		       int max_line_length)
  {
    pp_set_prefix (this, xstrdup ("PREFIX: "));
    wrapping.rule = rule;
    pp_set_line_maximum_length (this, max_line_length);
  }
};

/* Verify that the various values of enum diagnostic_prefixing_rule_t work
   as expected, with and without line wrapping.  */

static void
test_prefixes_and_wrapping ()
{
  /* Tests of the various prefixing rules, without wrapping.
     Newlines embedded in pp_string don't affect it; we have to
     explicitly call pp_newline.  */
  {
    test_pretty_printer pp (DIAGNOSTICS_SHOW_PREFIX_ONCE, 0);
    pp_string (&pp, "the quick brown fox");
    pp_newline (&pp);
    pp_string (&pp, "jumps over the lazy dog");
    pp_newline (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "PREFIX: the quick brown fox\n"
		  "   jumps over the lazy dog\n");
  }
  {
    test_pretty_printer pp (DIAGNOSTICS_SHOW_PREFIX_NEVER, 0);
    pp_string (&pp, "the quick brown fox");
    pp_newline (&pp);
    pp_string (&pp, "jumps over the lazy dog");
    pp_newline (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "the quick brown fox\n"
		  "jumps over the lazy dog\n");
  }
  {
    test_pretty_printer pp (DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE, 0);
    pp_string (&pp, "the quick brown fox");
    pp_newline (&pp);
    pp_string (&pp, "jumps over the lazy dog");
    pp_newline (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "PREFIX: the quick brown fox\n"
		  "PREFIX: jumps over the lazy dog\n");
  }

  /* Tests of the various prefixing rules, with wrapping.  */
  {
    test_pretty_printer pp (DIAGNOSTICS_SHOW_PREFIX_ONCE, 20);
    pp_string (&pp, "the quick brown fox jumps over the lazy dog");
    pp_newline (&pp);
    pp_string (&pp, "able was I ere I saw elba");
    pp_newline (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "PREFIX: the quick \n"
		  "   brown fox jumps \n"
		  "   over the lazy \n"
		  "   dog\n"
		  "   able was I ere I \n"
		  "   saw elba\n");
  }
  {
    test_pretty_printer pp (DIAGNOSTICS_SHOW_PREFIX_NEVER, 20);
    pp_string (&pp, "the quick brown fox jumps over the lazy dog");
    pp_newline (&pp);
    pp_string (&pp, "able was I ere I saw elba");
    pp_newline (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "the quick brown fox \n"
		  "jumps over the lazy \n"
		  "dog\n"
		  "able was I ere I \n"
		  "saw elba\n");
  }
  {
    test_pretty_printer pp (DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE, 20);
    pp_string (&pp, "the quick brown fox jumps over the lazy dog");
    pp_newline (&pp);
    pp_string (&pp, "able was I ere I saw elba");
    pp_newline (&pp);
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "PREFIX: the quick brown fox jumps over the lazy dog\n"
		  "PREFIX: able was I ere I saw elba\n");
  }

}

/* Verify that URL-printing works as expected.  */

void
test_urls ()
{
  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_NONE;
    pp_begin_url (&pp, "http://example.com");
    pp_string (&pp, "This is a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("This is a link",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_ST;
    pp_begin_url (&pp, "http://example.com");
    pp_string (&pp, "This is a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("\33]8;;http://example.com\33\\This is a link\33]8;;\33\\",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_BEL;
    pp_begin_url (&pp, "http://example.com");
    pp_string (&pp, "This is a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("\33]8;;http://example.com\aThis is a link\33]8;;\a",
		  pp_formatted_text (&pp));
  }
}

/* Verify that we gracefully reject null URLs.  */

void
test_null_urls ()
{
  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_NONE;
    pp_begin_url (&pp, nullptr);
    pp_string (&pp, "This isn't a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("This isn't a link",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_ST;
    pp_begin_url (&pp, nullptr);
    pp_string (&pp, "This isn't a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("This isn't a link",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_BEL;
    pp_begin_url (&pp, nullptr);
    pp_string (&pp, "This isn't a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("This isn't a link",
		  pp_formatted_text (&pp));
  }
}

/* Verify that URLification works as expected.  */

static void
pp_printf_with_urlifier (pretty_printer *pp,
			 const urlifier *urlifier,
			 const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  text_info text (msg, &ap, errno);
  pp_format (pp, &text, urlifier);
  pp_output_formatted_text (pp, urlifier);
  va_end (ap);
}


void
test_urlification ()
{
  class test_urlifier : public urlifier
  {
  public:
    char *
    get_url_for_quoted_text (const char *p, size_t sz) const final override
    {
      if (!strncmp (p, "-foption", sz))
	return xstrdup ("http://example.com");
      return nullptr;
    }
  };

  auto_fix_quotes fix_quotes;
  const test_urlifier urlifier;

  /* Uses of "%<" and "%>".  */
  {
    {
      pretty_printer pp;
      pp.url_format = URL_FORMAT_NONE;
      pp_printf_with_urlifier (&pp, &urlifier,
			       "foo %<-foption%> %<unrecognized%> bar");
      ASSERT_STREQ ("foo `-foption' `unrecognized' bar",
		    pp_formatted_text (&pp));
    }
    {
      pretty_printer pp;
      pp.url_format = URL_FORMAT_ST;
      pp_printf_with_urlifier (&pp, &urlifier,
			       "foo %<-foption%> %<unrecognized%> bar");
      ASSERT_STREQ
	("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\'"
	 " `unrecognized' bar",
	 pp_formatted_text (&pp));
    }
    {
      pretty_printer pp;
      pp.url_format = URL_FORMAT_BEL;
      pp_printf_with_urlifier (&pp, &urlifier,
			       "foo %<-foption%> %<unrecognized%> bar");
      ASSERT_STREQ
	("foo `\33]8;;http://example.com\a-foption\33]8;;\a'"
	 " `unrecognized' bar",
	 pp_formatted_text (&pp));
    }
  }

  /* Use of "%qs".  */
  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_ST;
    pp_printf_with_urlifier (&pp, &urlifier,
			     "foo %qs %qs bar",
			     "-foption", "unrecognized");
    ASSERT_STREQ
      ("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\'"
       " `unrecognized' bar",
       pp_formatted_text (&pp));
  }

  /* Mixed usage of %< and %s, where the quoted string is built between
     a mixture of phase 1 and phase 2.  */
  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_ST;
    pp_printf_with_urlifier (&pp, &urlifier,
			     "foo %<-f%s%> bar",
			     "option");
    ASSERT_STREQ
      ("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' bar",
       pp_formatted_text (&pp));
  }

  /* Likewise, where there is trailing phase 1 content within the
     quoted region.  */
  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_ST;
    pp_printf_with_urlifier (&pp, &urlifier,
			     "foo %<-f%sion%> bar %<-f%sion%> baz",
			     "opt", "opt");
    ASSERT_STREQ
      ("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' bar `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' baz",
       pp_formatted_text (&pp));
  }

  /* Likewise.  */
  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_ST;
    pp_printf_with_urlifier (&pp, &urlifier,
			     "foo %<%sption%> bar %<-f%sion%> baz",
			     "-fo", "opt");
    ASSERT_STREQ
      ("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' bar `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' baz",
       pp_formatted_text (&pp));
  }

  /* Another mixed usage of %< and %s, where the quoted string is built
     between a mixture of phase 1 and multiple phase 2.  */
  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_ST;
    pp_printf_with_urlifier (&pp, &urlifier,
			     "foo %<-f%s%s%> bar",
			     "opt", "ion");
    ASSERT_STREQ
      ("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' bar",
       pp_formatted_text (&pp));
  }

  /* Mixed usage of %< and %s with a prefix.  */
  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_ST;
    pp_set_prefix (&pp, xstrdup ("PREFIX"));
    pp_printf_with_urlifier (&pp, &urlifier,
			     "foo %<-f%s%> bar",
			     "option");
    ASSERT_STREQ
      ("PREFIXfoo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' bar",
       pp_formatted_text (&pp));
  }

  /* Example of mixed %< and %s with numbered args.  */
  {
    pretty_printer pp;
    pp.url_format = URL_FORMAT_ST;
    pp_printf_with_urlifier (&pp, &urlifier,
			     "foo %<-f%2$st%1$sn%> bar",
			     "io", "op");
    ASSERT_STREQ
      ("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' bar",
       pp_formatted_text (&pp));
  }
}

/* Test multibyte awareness.  */
static void test_utf8 ()
{

  /* Check that pp_quoted_string leaves valid UTF-8 alone.  */
  {
    pretty_printer pp;
    const char *s = "\xf0\x9f\x98\x82";
    pp_quoted_string (&pp, s);
    ASSERT_STREQ (pp_formatted_text (&pp), s);
  }

  /* Check that pp_quoted_string escapes non-UTF-8 nonprintable bytes.  */
  {
    pretty_printer pp;
    pp_quoted_string (&pp, "\xf0!\x9f\x98\x82");
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "\\xf0!\\x9f\\x98\\x82");
  }

  /* Check that pp_character will line-wrap at the beginning of a UTF-8
     sequence, but not in the middle.  */
  {
      pretty_printer pp (3);
      const char s[] = "---\xf0\x9f\x98\x82";
      for (int i = 0; i != sizeof (s) - 1; ++i)
	pp_character (&pp, s[i]);
      pp_newline (&pp);
      for (int i = 1; i != sizeof (s) - 1; ++i)
	pp_character (&pp, s[i]);
      pp_character (&pp, '-');
      ASSERT_STREQ (pp_formatted_text (&pp),
		    "---\n"
		    "\xf0\x9f\x98\x82\n"
		    "--\xf0\x9f\x98\x82\n"
		    "-");
  }

}

/* Run all of the selftests within this file.  */

void
pretty_print_cc_tests ()
{
  test_basic_printing ();
  test_pp_format ();
  test_prefixes_and_wrapping ();
  test_urls ();
  test_null_urls ();
  test_urlification ();
  test_utf8 ();
}

} // namespace selftest

#endif /* CHECKING_P */
