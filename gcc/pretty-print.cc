/* Various declarations for language-independent pretty-print subroutines.
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
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
#include "pretty-print-format-impl.h"
#include "pretty-print-markup.h"
#include "pretty-print-urlifier.h"
#include "diagnostic-color.h"
#include "diagnostic-event-id.h"
#include "diagnostic-highlight-colors.h"
#include "make-unique.h"
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

  if (GetConsoleMode (h, &mode)
#ifdef ENABLE_VIRTUAL_TERMINAL_PROCESSING
      && !(mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING)
#endif
      )
    /* If it is a console, and doesn't support ANSI escape codes, translate
       them as needed.  */
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

static void
default_token_printer (pretty_printer *pp,
		       const pp_token_list &tokens);

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
  : m_formatted_obstack (),
    m_chunk_obstack (),
    m_obstack (&m_formatted_obstack),
    m_cur_formatted_chunks (nullptr),
    m_stream (stderr),
    m_line_length (),
    m_digit_buffer (),
    m_flush_p (true)
{
  obstack_init (&m_formatted_obstack);
  obstack_init (&m_chunk_obstack);
}

// Release resources owned by an output buffer at the end of lifetime.

output_buffer::~output_buffer ()
{
  obstack_free (&m_chunk_obstack, NULL);
  obstack_free (&m_formatted_obstack, NULL);
}

/* Allocate a new pp_formatted_chunks from chunk_obstack and push
   it onto this buffer's stack.
   This represents the result of phases 1 and 2 of formatting.  */

pp_formatted_chunks *
output_buffer::push_formatted_chunks ()
{
  /* Allocate a new chunk structure.  */
  pp_formatted_chunks *new_chunk_array
    = XOBNEW (&m_chunk_obstack, pp_formatted_chunks);
  new_chunk_array->m_prev = m_cur_formatted_chunks;
  m_cur_formatted_chunks = new_chunk_array;
  return new_chunk_array;
}

/* Deallocate the current pp_formatted_chunks structure and everything after it
   (i.e. the associated series of formatted strings, pp_token_lists, and
   pp_tokens).  */

void
output_buffer::pop_formatted_chunks ()
{
  pp_formatted_chunks *old_top = m_cur_formatted_chunks;
  gcc_assert (old_top);
  m_cur_formatted_chunks = old_top->m_prev;
  obstack_free (&m_chunk_obstack, old_top);
}

static const int bytes_per_hexdump_line = 16;

static void
print_hexdump_line (FILE *out, int indent,
		    const void *buf, size_t size, size_t line_start_idx)
{
  fprintf (out, "%*s%08lx: ", indent, "", (unsigned long)line_start_idx);
  for (size_t offset = 0; offset < bytes_per_hexdump_line; ++offset)
    {
      const size_t idx = line_start_idx + offset;
      if (idx < size)
	fprintf (out, "%02x ", ((const unsigned char *)buf)[idx]);
      else
	fprintf (out, "   ");
    }
  fprintf (out, "| ");
  for (size_t offset = 0; offset < bytes_per_hexdump_line; ++offset)
    {
      const size_t idx = line_start_idx + offset;
      if (idx < size)
	{
	  unsigned char ch = ((const unsigned char *)buf)[idx];
	  if (!ISPRINT (ch))
	    ch = '.';
	  fputc (ch, out);
	}
      else
	break;
    }
  fprintf (out, "\n");

}

static void
print_hexdump (FILE *out, int indent, const void *buf, size_t size)
{
  for (size_t idx = 0; idx < size; idx += bytes_per_hexdump_line)
    print_hexdump_line (out, indent, buf, size, idx);
}

/* Dump state of this output_buffer to OUT, for debugging.  */

void
output_buffer::dump (FILE *out, int indent) const
{
  {
    size_t obj_size = obstack_object_size (&m_formatted_obstack);
    fprintf (out, "%*sm_formatted_obstack current object: length %li:\n",
	     indent, "", (long)obj_size);
    print_hexdump (out, indent + 2,
		   m_formatted_obstack.object_base, obj_size);
  }
  {
    size_t obj_size = obstack_object_size (&m_chunk_obstack);
    fprintf (out, "%*sm_chunk_obstack current object: length %li:\n",
	     indent, "", (long)obj_size);
    print_hexdump (out, indent + 2,
		   m_chunk_obstack.object_base, obj_size);
  }

  int depth = 0;
  for (pp_formatted_chunks *iter = m_cur_formatted_chunks;
       iter;
       iter = iter->m_prev, depth++)
    {
      fprintf (out, "%*spp_formatted_chunks: depth %i\n",
	       indent, "",
	       depth);
      iter->dump (out, indent + 2);
    }
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

void
pretty_printer::set_real_maximum_length ()
{
  /* If we're told not to wrap lines then do the obvious thing.  In case
     we'll emit prefix only once per message, it is appropriate
     not to increase unnecessarily the line-length cut-off.  */
  if (!pp_is_wrapping_line (this)
      || pp_prefixing_rule (this) == DIAGNOSTICS_SHOW_PREFIX_ONCE
      || pp_prefixing_rule (this) == DIAGNOSTICS_SHOW_PREFIX_NEVER)
    m_maximum_length = pp_line_cutoff (this);
  else
    {
      int prefix_length = m_prefix ? strlen (m_prefix) : 0;
      /* If the prefix is ridiculously too long, output at least
         32 characters.  */
      if (pp_line_cutoff (this) - prefix_length < 32)
	m_maximum_length = pp_line_cutoff (this) + 32;
      else
	m_maximum_length = pp_line_cutoff (this);
    }
}

/* Clear this pretty_printer's output state.  */
inline void
pretty_printer::clear_state ()
{
  m_emitted_prefix = false;
  pp_indentation (this) = 0;
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
  mingw_ansi_fputs (text, pp_buffer (pp)->m_stream);
#else
  fputs (text, pp_buffer (pp)->m_stream);
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
  FILE *fp = pp_buffer (pp)->m_stream;

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
  FILE *fp = pp_buffer (pp)->m_stream;

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
	    && p - start >= pp->remaining_character_count_for_line ())
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

/* struct pp_token.  */

pp_token::pp_token (enum kind k)
: m_kind (k),
  m_prev (nullptr),
  m_next (nullptr)
{
}

void
pp_token::dump (FILE *out) const
{
  switch (m_kind)
    {
    default:
      gcc_unreachable ();
    case kind::text:
      {
	const pp_token_text *sub = as_a <const pp_token_text *> (this);
	gcc_assert (sub->m_value.get ());
	fprintf (out, "TEXT(\"%s\")", sub->m_value.get ());
      }
      break;
    case kind::begin_color:
      {
	const pp_token_begin_color *sub
	  = as_a <const pp_token_begin_color *> (this);
	gcc_assert (sub->m_value.get ());
	fprintf (out, "BEGIN_COLOR(\"%s\")", sub->m_value.get ());
	break;
      }
    case kind::end_color:
      fprintf (out, "END_COLOR");
      break;
    case kind::begin_quote:
      fprintf (out, "BEGIN_QUOTE");
      break;
    case kind::end_quote:
      fprintf (out, "END_QUOTE");
      break;
    case kind::begin_url:
      {
	const pp_token_begin_url *sub
	  = as_a <const pp_token_begin_url *> (this);
	gcc_assert (sub->m_value.get ());
	fprintf (out, "BEGIN_URL(\"%s\")", sub->m_value.get ());
      }
      break;
    case kind::end_url:
      fprintf (out, "END_URL");
      break;

    case kind::event_id:
      {
	const pp_token_event_id *sub
	  = as_a <const pp_token_event_id *> (this);
	gcc_assert (sub->m_event_id.known_p ());
	fprintf (out, "EVENT((%i))", sub->m_event_id.one_based ());
      }
      break;

    case kind::custom_data:
      {
	const pp_token_custom_data *sub
	  = as_a <const pp_token_custom_data *> (this);
	gcc_assert (sub->m_value.get ());
	fprintf (out, "CUSTOM(");
	sub->m_value->dump (out);
	fprintf (out, ")");
      }
      break;
    }
}

/* Allocate SZ bytes within S, which must not be half-way through
   building another object.  */

static void *
allocate_object (size_t sz, obstack &s)
{
  /* We must not be half-way through an object.  */
  gcc_assert (obstack_base (&s) == obstack_next_free (&s));

  obstack_blank (&s, sz);
  void *buf = obstack_finish (&s);
  return buf;
}

/* Make room for a pp_token instance within obstack S.  */

void *
pp_token::operator new (size_t sz, obstack &s)
{
  return allocate_object (sz, s);
}

void
pp_token::operator delete (void *)
{
  /* No-op: pp_tokens are allocated within obstacks, so
     the memory will be reclaimed when the obstack is freed.  */
}

/* class pp_token_list.  */

/* Make room for a pp_token_list instance within obstack S.  */

void *
pp_token_list::operator new (size_t sz, obstack &s)
{
  return allocate_object (sz, s);
}

void
pp_token_list::operator delete (void *)
{
  /* No-op: pp_token_list allocated within obstacks don't
     need their own reclaim the memory will be reclaimed when
     the obstack is freed.  */
}

pp_token_list::pp_token_list (obstack &s)
: m_obstack (s),
  m_first (nullptr),
  m_end (nullptr)
{
}

pp_token_list::pp_token_list (pp_token_list &&other)
: m_obstack (other.m_obstack),
  m_first (other.m_first),
  m_end (other.m_end)
{
  other.m_first = nullptr;
  other.m_end = nullptr;
}

pp_token_list::~pp_token_list ()
{
  for (auto iter = m_first; iter; )
    {
      pp_token *next = iter->m_next;
      delete iter;
      iter = next;
    }
}

void
pp_token_list::push_back_text (label_text &&text)
{
  if (text.get ()[0] == '\0')
    return; // pushing empty string is a no-op
  push_back<pp_token_text> (std::move (text));
}

void
pp_token_list::push_back (std::unique_ptr<pp_token> tok)
{
  if (!m_first)
    {
      gcc_assert (m_end == nullptr);
      m_first = tok.get ();
      m_end = tok.get ();
    }
  else
    {
      gcc_assert (m_end != nullptr);
      m_end->m_next = tok.get ();
      tok->m_prev = m_end;
      m_end = tok.get ();
    }
  tok.release ();
}

void
pp_token_list::push_back_list (pp_token_list &&list)
{
  while (auto tok = list.pop_front ())
    push_back (std::move (tok));
}

std::unique_ptr<pp_token>
pp_token_list::pop_front ()
{
  pp_token *result = m_first;
  if (result == nullptr)
    return nullptr;

  gcc_assert (result->m_prev == nullptr);
  m_first = result->m_next;
  if (result->m_next)
    {
      gcc_assert (result != m_end);
      m_first->m_prev = nullptr;
    }
  else
    {
      gcc_assert (result == m_end);
      m_end = nullptr;
    }
  result->m_next = nullptr;
  return std::unique_ptr<pp_token> (result);
}

std::unique_ptr<pp_token>
pp_token_list::remove_token (pp_token *tok)
{
  gcc_assert (tok);
  if (tok->m_prev)
    {
      gcc_assert (tok != m_first);
      tok->m_prev->m_next = tok->m_next;
    }
  else
    {
      gcc_assert (tok == m_first);
      m_first = tok->m_next;
    }
  if (tok->m_next)
    {
      gcc_assert (tok != m_end);
      tok->m_next->m_prev = tok->m_prev;
    }
  else
    {
      gcc_assert (tok == m_end);
      m_end = tok->m_prev;
    }
  tok->m_prev = nullptr;
  tok->m_next = nullptr;
  gcc_assert (m_first != tok);
  gcc_assert (m_end != tok);
  return std::unique_ptr<pp_token> (tok);
}

/* Insert NEW_TOK after RELATIVE_TOK.  */

void
pp_token_list::insert_after (std::unique_ptr<pp_token> new_tok_up,
			     pp_token *relative_tok)
{
  pp_token *new_tok = new_tok_up.release ();

  gcc_assert (new_tok);
  gcc_assert (new_tok->m_prev == nullptr);
  gcc_assert (new_tok->m_next == nullptr);
  gcc_assert (relative_tok);

  if (relative_tok->m_next)
    {
      gcc_assert (relative_tok != m_end);
      relative_tok->m_next->m_prev = new_tok;
    }
  else
    {
      gcc_assert (relative_tok == m_end);
      m_end = new_tok;
    }
  new_tok->m_prev = relative_tok;
  new_tok->m_next = relative_tok->m_next;
  relative_tok->m_next = new_tok;
}

void
pp_token_list::replace_custom_tokens ()
{
  pp_token *iter = m_first;
  while (iter)
    {
      pp_token *next  = iter->m_next;
      if (iter->m_kind == pp_token::kind::custom_data)
	{
	  pp_token_list tok_list (m_obstack);
	  pp_token_custom_data *sub = as_a <pp_token_custom_data *> (iter);
	  if (sub->m_value->as_standard_tokens (tok_list))
	    {
	      while (auto tok = tok_list.pop_front ())
		{
		  /* The resulting token list must not contain any
		     custom data.  */
		  gcc_assert (tok->m_kind != pp_token::kind::custom_data);
		  insert_after (std::move (tok), iter);
		}
	      remove_token (iter);
	    }
	}
      iter = next;
    }
}

/* Merge any runs of consecutive text tokens within this list
   into individual text tokens.  */

void
pp_token_list::merge_consecutive_text_tokens ()
{
  pp_token *start_of_run = m_first;
  while (start_of_run)
    {
      if (start_of_run->m_kind != pp_token::kind::text)
	{
	  start_of_run = start_of_run->m_next;
	  continue;
	}
      pp_token *end_of_run = start_of_run;
      while (end_of_run->m_next
	     && end_of_run->m_next->m_kind == pp_token::kind::text)
	end_of_run = end_of_run->m_next;
      if (end_of_run != start_of_run)
	{
	  /* start_of_run through end_of_run are a run of consecutive
	     text tokens.  */

	  /* Calculate size of buffer for merged text.  */
	  size_t sz = 0;
	  for (auto iter = start_of_run; iter != end_of_run->m_next;
	       iter = iter->m_next)
	    {
	      pp_token_text *iter_text = static_cast<pp_token_text *> (iter);
	      sz += strlen (iter_text->m_value.get ());
	    }

	  /* Allocate and populate buffer for merged text
	     (within m_obstack).  */
	  char * const buf = (char *)allocate_object (sz + 1, m_obstack);
	  char *p = buf;
	  for (auto iter = start_of_run; iter != end_of_run->m_next;
	       iter = iter->m_next)
	    {
	      pp_token_text *iter_text = static_cast<pp_token_text *> (iter);
	      size_t iter_sz = strlen (iter_text->m_value.get ());
	      memcpy (p, iter_text->m_value.get (), iter_sz);
	      p += iter_sz;
	    }
	  *p = '\0';

	  /* Replace start_of_run's buffer pointer with the new buffer.  */
	  static_cast<pp_token_text *> (start_of_run)->m_value
	    = label_text::borrow (buf);

	  /* Remove all the other text tokens in the run.  */
	  pp_token * const next = end_of_run->m_next;
	  while (start_of_run->m_next != next)
	    remove_token (start_of_run->m_next);
	  start_of_run = next;
	}
      else
	start_of_run = end_of_run->m_next;
    }
}

/* Apply URLIFIER to this token list.
   Find BEGIN_QUOTE, TEXT, END_QUOTE triples, and if URLIFIER has a url
   for the value of TEXT, then wrap TEXT in a {BEGIN,END}_URL pair.  */

void
pp_token_list::apply_urlifier (const urlifier &urlifier)
{
  for (pp_token *iter = m_first; iter; )
    {
      if (iter->m_kind == pp_token::kind::begin_quote
	  && iter->m_next
	  && iter->m_next->m_kind == pp_token::kind::text
	  && iter->m_next->m_next
	  && iter->m_next->m_next->m_kind == pp_token::kind::end_quote)
	{
	  pp_token *begin_quote = iter;
	  pp_token_text *text = as_a <pp_token_text *> (begin_quote->m_next);
	  pp_token *end_quote = text->m_next;
	  if (char *url = urlifier.get_url_for_quoted_text
			    (text->m_value.get (),
			     strlen (text->m_value.get ())))
	    {
	      auto begin_url
		= make_token<pp_token_begin_url> (label_text::take (url));
	      auto end_url = make_token<pp_token_end_url> ();
	      insert_after (std::move (begin_url), begin_quote);
	      insert_after (std::move (end_url), text);
	    }
	  iter = end_quote->m_next;
	}
      else
	iter = iter->m_next;
    }
}

void
pp_token_list::dump (FILE *out) const
{
  for (auto iter = m_first; iter; iter = iter->m_next)
    {
      iter->dump (out);
      if (iter->m_next)
	fprintf (out, ", ");
    }
  fprintf (out, "]\n");
}


/* Adds a chunk to the end of formatted output, so that it
   will be printed by pp_output_formatted_text.  */

void
pp_formatted_chunks::append_formatted_chunk (obstack &s, const char *content)
{
  unsigned int chunk_idx;
  for (chunk_idx = 0; m_args[chunk_idx]; chunk_idx++)
    ;
  pp_token_list *tokens = pp_token_list::make (s);
  tokens->push_back_text (label_text::borrow (content));
  m_args[chunk_idx++] = tokens;
  m_args[chunk_idx] = nullptr;
}

void
pp_formatted_chunks::dump (FILE *out, int indent) const
{
  for (size_t idx = 0; m_args[idx]; ++idx)
    {
      fprintf (out, "%*s%i: ",
	       indent, "",
	       (int)idx);
      m_args[idx]->dump (out);
    }
}

/* Finish any text accumulating within CUR_OBSTACK,
   terminating it.
   Push a text pp_token to the end of TOK_LIST containing
   a borrowed copy of the text in CUR_OBSTACK.  */

static void
push_back_any_text (pp_token_list *tok_list,
		    obstack *cur_obstack)
{
  obstack_1grow (cur_obstack, '\0');
  tok_list->push_back_text
    (label_text::borrow (XOBFINISH (cur_obstack,
				    const char *)));
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

   %e: Consumes a pp_element * argument.

   Arguments can be used sequentially, or through %N$ resp. *N$
   notation Nth argument after the format string.  If %N$ / *N$
   notation is used, it must be used for all arguments, except %m, %%,
   %<, %>, %} and %', which may not have a number, as they do not consume
   an argument.  When %M$.*N$s is used, M must be N + 1.  (This may
   also be written %M$.*s, provided N is not otherwise used.)  The
   format string must have conversion specifiers with argument numbers
   1 up to highest argument; each argument may only be used once.
   A format string can have at most 30 arguments.  */

/* Implementation of pp_format.
   Formatting phases 1 and 2:
   - push a pp_formatted_chunks instance.
   - render TEXT->format_spec plus text->m_args_ptr into the pp_formatted_chunks
     instance as pp_token_lists.
   Phase 3 is in pp_output_formatted_text, which pops the pp_formatted_chunks
   instance.  */

static void
format_phase_1 (const text_info &text,
		obstack &chunk_obstack,
		pp_token_list **args,
		pp_token_list ***formatters);

static void
format_phase_2 (pretty_printer *pp,
		text_info &text,
		obstack &chunk_obstack,
		pp_token_list ***formatters);

void
pretty_printer::format (text_info &text)
{
  pp_formatted_chunks *new_chunk_array = m_buffer->push_formatted_chunks ();
  pp_token_list **args = new_chunk_array->m_args;

  pp_token_list **formatters[PP_NL_ARGMAX];
  memset (formatters, 0, sizeof formatters);

  /* Formatting phase 1: split up TEXT->format_spec into chunks in
     pp_buffer (PP)->args[].  Even-numbered chunks are to be output
     verbatim, odd-numbered chunks are format specifiers.
     %m, %%, %<, %>, %} and %' are replaced with the appropriate text at
     this point.  */
  format_phase_1 (text, m_buffer->m_chunk_obstack, args, formatters);

  /* Note that you can debug the state of the chunk arrays here using
       (gdb) call m_buffer->cur_chunk_array->dump()
     which, given e.g. "foo: %s bar: %s" might print:
       0: [TEXT("foo: ")]
       1: [TEXT("s")]
       2: [TEXT(" bar: ")]
       3: [TEXT("s")]
  */

  /* Set output to the argument obstack, and switch line-wrapping and
     prefixing off.  */
  m_buffer->m_obstack = &m_buffer->m_chunk_obstack;
  const int old_line_length = m_buffer->m_line_length;
  const pp_wrapping_mode_t old_wrapping_mode = pp_set_verbatim_wrapping (this);

  format_phase_2 (this, text, m_buffer->m_chunk_obstack, formatters);

  /* If the client supplied a postprocessing object, call its "handle"
     hook here.  */
  if (m_format_postprocessor)
    m_format_postprocessor->handle (this);

  /* Revert to normal obstack and wrapping mode.  */
  m_buffer->m_obstack = &m_buffer->m_formatted_obstack;
  m_buffer->m_line_length = old_line_length;
  pp_wrapping_mode (this) = old_wrapping_mode;
  clear_state ();
}

static void
format_phase_1 (const text_info &text,
		obstack &chunk_obstack,
		pp_token_list **args,
		pp_token_list ***formatters)
{
  unsigned chunk = 0;
  unsigned int curarg = 0;
  bool any_unnumbered = false, any_numbered = false;
  pp_token_list *cur_token_list;
  args[chunk++] = cur_token_list = pp_token_list::make (chunk_obstack);
  for (const char *p = text.m_format_spec; *p; )
    {
      while (*p != '\0' && *p != '%')
	{
	  obstack_1grow (&chunk_obstack, *p);
	  p++;
	}

      if (*p == '\0')
	break;

      switch (*++p)
	{
	case '\0':
	  gcc_unreachable ();

	case '%':
	  obstack_1grow (&chunk_obstack, '%');
	  p++;
	  continue;

	case '<':
	  {
	    push_back_any_text (cur_token_list, &chunk_obstack);
	    cur_token_list->push_back<pp_token_begin_quote> ();
	    p++;
	    continue;
	  }

	case '>':
	  {
	    push_back_any_text (cur_token_list, &chunk_obstack);
	    cur_token_list->push_back<pp_token_end_quote> ();
	    p++;
	    continue;
	  }
	case '\'':
	  {
	    push_back_any_text (cur_token_list, &chunk_obstack);
	    cur_token_list->push_back<pp_token_end_quote> ();
	    p++;
	  }
	  continue;

	case '}':
	  {
	    push_back_any_text (cur_token_list, &chunk_obstack);
	    cur_token_list->push_back<pp_token_end_url> ();
	    p++;
	  }
	  continue;

	case 'R':
	  {
	    push_back_any_text (cur_token_list, &chunk_obstack);
	    cur_token_list->push_back<pp_token_end_color> ();
	    p++;
	    continue;
	  }

	case 'm':
	  {
	    const char *errstr = xstrerror (text.m_err_no);
	    obstack_grow (&chunk_obstack, errstr, strlen (errstr));
	  }
	  p++;
	  continue;

	default:
	  /* Handled in phase 2.  Terminate the plain chunk here.  */
	  push_back_any_text (cur_token_list, &chunk_obstack);
	  break;
	}

      /* Start a new token list for the formatting args.  */
      args[chunk] = cur_token_list = pp_token_list::make (chunk_obstack);

      unsigned argno;
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
      formatters[argno] = &args[chunk++];
      do
	{
	  obstack_1grow (&chunk_obstack, *p);
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
		  obstack_1grow (&chunk_obstack, *p);
		  p++;
		}
	      while (ISDIGIT (p[-1]));
	      gcc_assert (p[-1] == 's');
	    }
	  else
	    {
	      gcc_assert (*p == '*');
	      obstack_1grow (&chunk_obstack, '*');
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
	      obstack_1grow (&chunk_obstack, 's');
	      p++;
	    }
	}
      if (*p == '\0')
	{
	  push_back_any_text (cur_token_list, &chunk_obstack);
	  break;
	}

      obstack_1grow (&chunk_obstack, '\0');
      push_back_any_text (cur_token_list, &chunk_obstack);

      /* Start a new token list for the next (non-formatted) text.  */
      gcc_assert (chunk < PP_NL_ARGMAX * 2);
      args[chunk++] = cur_token_list = pp_token_list::make (chunk_obstack);
    }

  obstack_1grow (&chunk_obstack, '\0');
  push_back_any_text (cur_token_list, &chunk_obstack);
  gcc_assert (chunk < PP_NL_ARGMAX * 2);
  args[chunk] = nullptr;
}

/* Second phase.  Replace each formatter with pp_tokens for the formatted
   text it corresponds to, consuming va_args from TEXT->m_args_ptr.  */

static void
format_phase_2 (pretty_printer *pp,
		text_info &text,
		obstack &chunk_obstack,
		pp_token_list ***formatters)
{
  unsigned argno;
  for (argno = 0; formatters[argno]; argno++)
    {
      int precision = 0;
      bool wide = false;
      bool plus = false;
      bool hash = false;
      bool quote = false;

      /* We expect a single text token containing the formatter.  */
      pp_token_list *tok_list = *(formatters[argno]);
      gcc_assert (tok_list);
      gcc_assert (tok_list->m_first == tok_list->m_end);
      gcc_assert (tok_list->m_first->m_kind == pp_token::kind::text);

      /* Accumulate the value of the formatted text into here.  */
      pp_token_list *formatted_tok_list
	= pp_token_list::make (chunk_obstack);

      /* We do not attempt to enforce any ordering on the modifier
	 characters.  */

      const char *p;
      for (p = as_a <pp_token_text *> (tok_list->m_first)->m_value.get ();; p++)
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
	  push_back_any_text (formatted_tok_list, &chunk_obstack);
	  formatted_tok_list->push_back<pp_token_begin_quote> ();
	}

      switch (*p)
	{
	case 'r':
	  {
	    const char *color = va_arg (*text.m_args_ptr, const char *);
	    formatted_tok_list->push_back<pp_token_begin_color>
	      (label_text::borrow (color));
	  }
	  break;

	case 'c':
	  {
	    /* When quoting, print alphanumeric, punctuation, and the space
	       character unchanged, and all others in hexadecimal with the
	       "\x" prefix.  Otherwise print them all unchanged.  */
	    char chr = (char) va_arg (*text.m_args_ptr, int);
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
	    pp_wide_integer (pp, va_arg (*text.m_args_ptr, HOST_WIDE_INT));
	  else
	    pp_integer_with_precision (pp, *text.m_args_ptr, precision,
				       int, "d");
	  break;

	case 'o':
	  if (wide)
	    pp_scalar (pp, "%" HOST_WIDE_INT_PRINT "o",
		       va_arg (*text.m_args_ptr, unsigned HOST_WIDE_INT));
	  else
	    pp_integer_with_precision (pp, *text.m_args_ptr, precision,
				       unsigned, "o");
	  break;

	case 's':
	  if (quote)
	    pp_quoted_string (pp, va_arg (*text.m_args_ptr, const char *));
	  else
	    pp_string (pp, va_arg (*text.m_args_ptr, const char *));
	  break;

	case 'p':
	  pp_pointer (pp, va_arg (*text.m_args_ptr, void *));
	  break;

	case 'u':
	  if (wide)
	    pp_scalar (pp, HOST_WIDE_INT_PRINT_UNSIGNED,
		       va_arg (*text.m_args_ptr, unsigned HOST_WIDE_INT));
	  else
	    pp_integer_with_precision (pp, *text.m_args_ptr, precision,
				       unsigned, "u");
	  break;

	case 'f':
	  pp_double (pp, va_arg (*text.m_args_ptr, double));
	  break;

	case 'Z':
	  {
	    int *v = va_arg (*text.m_args_ptr, int *);
	    unsigned len = va_arg (*text.m_args_ptr, unsigned);

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
		       va_arg (*text.m_args_ptr, unsigned HOST_WIDE_INT));
	  else
	    pp_integer_with_precision (pp, *text.m_args_ptr, precision,
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
		n = va_arg (*text.m_args_ptr, int);

		/* This consumes a second entry in the formatters array.  */
		gcc_assert (formatters[argno] == formatters[argno+1]);
		argno++;
	      }

	    s = va_arg (*text.m_args_ptr, const char *);

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
	      = va_arg (*text.m_args_ptr, diagnostic_event_id_ptr);
	    gcc_assert (event_id->known_p ());
	    formatted_tok_list->push_back<pp_token_event_id> (*event_id);
	  }
	  break;

	case '{':
	  {
	    const char *url = va_arg (*text.m_args_ptr, const char *);
	    formatted_tok_list->push_back<pp_token_begin_url>
	      (label_text::borrow (url));
	  }
	  break;

	case 'e':
	  {
	    pp_element *element = va_arg (*text.m_args_ptr, pp_element *);
	    pp_markup::context ctxt (*pp,
				     quote, /* by reference */
				     formatted_tok_list);
	    element->add_to_phase_2 (ctxt);
	  }
	  break;

	default:
	  {
	    /* Call the format decoder.
	       Pass the address of "quote" so that format decoders can
	       potentially disable printing of the closing quote
	       (e.g. when printing "'TYPEDEF' aka 'TYPE'" in the C family
	       of frontends).  */
	    printer_fn format_decoder = pp_format_decoder (pp);
	    gcc_assert (format_decoder);
	    gcc_assert (formatted_tok_list);
	    bool ok = format_decoder (pp, &text, p,
				      precision, wide, plus, hash, &quote,
				      *formatted_tok_list);
	    gcc_assert (ok);
	  }
	}

      if (quote)
	{
	  push_back_any_text (formatted_tok_list, &chunk_obstack);
	  formatted_tok_list->push_back<pp_token_end_quote> ();
	}

      push_back_any_text (formatted_tok_list, &chunk_obstack);
      delete *formatters[argno];
      *formatters[argno] = formatted_tok_list;
    }

  if (CHECKING_P)
    for (; argno < PP_NL_ARGMAX; argno++)
      gcc_assert (!formatters[argno]);
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

  operator obstack & () { return m_obstack; }

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

/* Phase 3 of formatting a message (phases 1 and 2 done by pp_format).

   Pop a pp_formatted_chunks from chunk_obstack, collecting all the tokens from
   phases 1 and 2 of formatting, and writing into text in formatted_obstack.

   If URLIFIER is non-null then use it on any quoted text that was not
   handled in phases 1 or 2 to potentially add URLs.  */

void
pp_output_formatted_text (pretty_printer *pp,
			  const urlifier *urlifier)
{
  output_buffer * const buffer = pp_buffer (pp);
  gcc_assert (buffer->m_obstack == &buffer->m_formatted_obstack);

  pp_formatted_chunks *chunk_array = buffer->m_cur_formatted_chunks;
  pp_token_list * const *token_lists = chunk_array->get_token_lists ();

  {
    /* Consolidate into one token list.  */
    pp_token_list tokens (buffer->m_chunk_obstack);
    for (unsigned chunk = 0; token_lists[chunk]; chunk++)
      {
	tokens.push_back_list (std::move (*token_lists[chunk]));
	delete token_lists[chunk];
      }

    tokens.replace_custom_tokens ();

    tokens.merge_consecutive_text_tokens ();

    if (urlifier)
      tokens.apply_urlifier (*urlifier);

    /* This is a third phase, first 2 phases done in pp_format_args.
       Now we actually print it.  */
    if (pp->m_token_printer)
      pp->m_token_printer->print_tokens (pp, tokens);
    else
      default_token_printer (pp, tokens);

  /* Close the scope here to ensure that "tokens" above is fully cleared up
     before popping the current pp_formatted_chunks, since that latter will pop
     the chunk_obstack, and "tokens" may be using blocks within
     the current pp_formatted_chunks's chunk_obstack level.  */
  }

  buffer->pop_formatted_chunks ();
}

/* Default implementation of token printing.  */

static void
default_token_printer (pretty_printer *pp,
		       const pp_token_list &tokens)
{
  /* Convert to text, possibly with colorization, URLs, etc.  */
  for (auto iter = tokens.m_first; iter; iter = iter->m_next)
    switch (iter->m_kind)
      {
      default:
	gcc_unreachable ();

      case pp_token::kind::text:
	{
	  pp_token_text *sub = as_a <pp_token_text *> (iter);
	  pp_string (pp, sub->m_value.get ());
	}
	break;

      case pp_token::kind::begin_color:
	{
	  pp_token_begin_color *sub = as_a <pp_token_begin_color *> (iter);
	  pp_string (pp, colorize_start (pp_show_color (pp),
					 sub->m_value.get ()));
	}
	break;
      case pp_token::kind::end_color:
	pp_string (pp, colorize_stop (pp_show_color (pp)));
	break;

      case pp_token::kind::begin_quote:
	pp_begin_quote (pp, pp_show_color (pp));
	break;
      case pp_token::kind::end_quote:
	pp_end_quote (pp, pp_show_color (pp));
	break;

      case pp_token::kind::begin_url:
	{
	  pp_token_begin_url *sub = as_a <pp_token_begin_url *> (iter);
	  pp_begin_url (pp, sub->m_value.get ());
	}
	break;
      case pp_token::kind::end_url:
	pp_end_url (pp);
	break;

      case pp_token::kind::event_id:
	{
	  pp_token_event_id *sub = as_a <pp_token_event_id *> (iter);
	  gcc_assert (sub->m_event_id.known_p ());
	  pp_string (pp, colorize_start (pp_show_color (pp), "path"));
	  pp_character (pp, '(');
	  pp_decimal_int (pp, sub->m_event_id.one_based ());
	  pp_character (pp, ')');
	  pp_string (pp, colorize_stop (pp_show_color (pp)));
	}
	break;

      case pp_token::kind::custom_data:
	/* These should have been eliminated by replace_custom_tokens.  */
	gcc_unreachable ();
	break;
      }
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
  pp->clear_state ();
  if (!pp_buffer (pp)->m_flush_p)
    return;
  pp_write_text_to_stream (pp);
  fflush (pp_buffer (pp)->m_stream);
}

/* Flush the content of BUFFER onto the attached stream independently
   of the value of pp->output_buffer->flush_p.  */
void
pp_really_flush (pretty_printer *pp)
{
  pp->clear_state ();
  pp_write_text_to_stream (pp);
  fflush (pp_buffer (pp)->m_stream);
}

/* Sets the number of maximum characters per line PRETTY-PRINTER can
   output in line-wrapping mode.  A LENGTH value 0 suppresses
   line-wrapping.  */
void
pp_set_line_maximum_length (pretty_printer *pp, int length)
{
  pp_line_cutoff (pp) = length;
  pp->set_real_maximum_length ();
}

/* Clear PRETTY-PRINTER output area text info.  */
void
pp_clear_output_area (pretty_printer *pp)
{
  obstack_free (pp_buffer (pp)->m_obstack,
		obstack_base (pp_buffer (pp)->m_obstack));
  pp_buffer (pp)->m_line_length = 0;
}

/* Set PREFIX for PRETTY-PRINTER, taking ownership of PREFIX, which
   will eventually be free-ed.  */

void
pretty_printer::set_prefix (char *prefix)
{
  free (m_prefix);
  m_prefix = prefix;
  set_real_maximum_length ();
  m_emitted_prefix = false;
  pp_indentation (this) = 0;
}

/* Take ownership of PP's prefix, setting it to NULL.
   This allows clients to save, override, and then restore an existing
   prefix, without it being free-ed.  */

char *
pp_take_prefix (pretty_printer *pp)
{
  char *result = pp->m_prefix;
  pp->m_prefix = nullptr;
  return result;
}

/* Free PRETTY-PRINTER's prefix, a previously malloc()'d string.  */
void
pp_destroy_prefix (pretty_printer *pp)
{
  if (pp->m_prefix)
    {
      free (pp->m_prefix);
      pp->m_prefix = nullptr;
    }
}

/* Write out this pretty_printer's prefix.  */
void
pretty_printer::emit_prefix ()
{
  if (m_prefix)
    {
      switch (pp_prefixing_rule (this))
	{
	default:
	case DIAGNOSTICS_SHOW_PREFIX_NEVER:
	  break;

	case DIAGNOSTICS_SHOW_PREFIX_ONCE:
	  if (m_emitted_prefix)
	    {
	      pp_indent (this);
	      break;
	    }
	  pp_indentation (this) += 3;
	  /* Fall through.  */

	case DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE:
	  {
	    int prefix_length = strlen (m_prefix);
	    pp_append_r (this, m_prefix, prefix_length);
	    m_emitted_prefix = true;
	  }
	  break;
	}
    }
}

/* Construct a PRETTY-PRINTER of MAXIMUM_LENGTH characters per line.  */

pretty_printer::pretty_printer (int maximum_length)
  : m_buffer (new (XCNEW (output_buffer)) output_buffer ()),
    m_prefix (nullptr),
    m_padding (pp_none),
    m_maximum_length (0),
    m_indent_skip (0),
    m_wrapping (),
    m_format_decoder (nullptr),
    m_format_postprocessor (NULL),
    m_token_printer (nullptr),
    m_emitted_prefix (false),
    m_need_newline (false),
    m_translate_identifiers (true),
    m_show_color (false),
    m_show_highlight_colors (false),
    m_url_format (URL_FORMAT_NONE),
    m_skipping_null_url (false)
{
  pp_line_cutoff (this) = maximum_length;
  /* By default, we emit prefixes once per message.  */
  pp_prefixing_rule (this) = DIAGNOSTICS_SHOW_PREFIX_ONCE;
  pp_set_prefix (this, NULL);
}

/* Copy constructor for pretty_printer.  */

pretty_printer::pretty_printer (const pretty_printer &other)
: m_buffer (new (XCNEW (output_buffer)) output_buffer ()),
  m_prefix (nullptr),
  m_padding (other.m_padding),
  m_maximum_length (other.m_maximum_length),
  m_indent_skip (other.m_indent_skip),
  m_wrapping (other.m_wrapping),
  m_format_decoder (other.m_format_decoder),
  m_format_postprocessor (NULL),
  m_token_printer (other.m_token_printer),
  m_emitted_prefix (other.m_emitted_prefix),
  m_need_newline (other.m_need_newline),
  m_translate_identifiers (other.m_translate_identifiers),
  m_show_color (other.m_show_color),
  m_show_highlight_colors (other.m_show_highlight_colors),
  m_url_format (other.m_url_format),
  m_skipping_null_url (false)
{
  pp_line_cutoff (this) = m_maximum_length;
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
  m_buffer->~output_buffer ();
  XDELETE (m_buffer);
  free (m_prefix);
}

/* Base class implementation of pretty_printer::clone vfunc.  */

std::unique_ptr<pretty_printer>
pretty_printer::clone () const
{
  return ::make_unique<pretty_printer> (*this);
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
  if (pp_buffer (pp)->m_line_length == 0)
    {
      pp->emit_prefix ();
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
pretty_printer::remaining_character_count_for_line ()
{
  return m_maximum_length - pp_buffer (this)->m_line_length;
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

/* Format a message into PP using ngettext to handle
   singular vs plural.  */

void
pp_printf_n (pretty_printer *pp,
	     unsigned HOST_WIDE_INT n,
	     const char *singular_gmsgid, const char *plural_gmsgid, ...)
{
  va_list ap;

  va_start (ap, plural_gmsgid);

  unsigned long gtn;
  if (sizeof n <= sizeof gtn)
    gtn = n;
  else
    /* Use the largest number ngettext can handle, otherwise
       preserve the six least significant decimal digits for
       languages where the plural form depends on them.  */
    gtn = n <= ULONG_MAX ? n : n % 1000000LU + 1000000LU;
  const char *msg = ngettext (singular_gmsgid, plural_gmsgid, gtn);
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
  obstack_1grow (pp_buffer (pp)->m_obstack, '\n');
  pp_needs_newline (pp) = false;
  pp_buffer (pp)->m_line_length = 0;
}

/* Have PRETTY-PRINTER add a CHARACTER.  */
void
pp_character (pretty_printer *pp, int c)
{
  if (pp_is_wrapping_line (pp)
      /* If printing UTF-8, don't wrap in the middle of a sequence.  */
      && (((unsigned int) c) & 0xC0) != 0x80
      && pp->remaining_character_count_for_line () <= 0)
    {
      pp_newline (pp);
      if (ISSPACE (c))
        return;
    }
  obstack_1grow (pp_buffer (pp)->m_obstack, c);
  ++pp_buffer (pp)->m_line_length;
}

/* Append a STRING to the output area of PRETTY-PRINTER; the STRING may
   be line-wrapped if in appropriate mode.  */
void
pp_string (pretty_printer *pp, const char *str)
{
  gcc_checking_assert (str);
  pp_maybe_wrap_text (pp, str, str + strlen (str));
}

/* As per pp_string, but only append the first LEN of STR.  */

void
pp_string_n (pretty_printer *pp, const char *str, size_t len)
{
  gcc_checking_assert (str);
  pp_maybe_wrap_text (pp, str, str + len);
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
pretty_printer::maybe_space ()
{
  if (m_padding != pp_none)
    {
      pp_space (this);
      m_padding = pp_none;
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
pretty_printer::begin_url (const char *url)
{
  if (!url)
    {
      /* Handle null URL by skipping all output here,
	 and in the next pp_end_url.  */
      m_skipping_null_url = true;
      return;
    }
  switch (m_url_format)
    {
    case URL_FORMAT_NONE:
      break;
    case URL_FORMAT_ST:
      pp_string (this, "\33]8;;");
      pp_string (this, url);
      pp_string (this, "\33\\");
      break;
    case URL_FORMAT_BEL:
      pp_string (this, "\33]8;;");
      pp_string (this, url);
      pp_string (this, "\a");
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
  switch (pp->get_url_format ())
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
pretty_printer::end_url ()
{
  if (m_skipping_null_url)
    {
      /* We gracefully handle pp_begin_url (NULL) by omitting output for
	 both begin and end.  Here we handle the latter.  */
      m_skipping_null_url = false;
      return;
    }
  if (m_url_format != URL_FORMAT_NONE)
    pp_string (this, get_end_url_string (this));
}

/* Dump state of this pretty_printer to OUT, for debugging.  */

void
pretty_printer::dump (FILE *out, int indent) const
{
  fprintf (out, "%*sm_show_color: %s\n",
	   indent, "",
	   m_show_color ? "true" : "false");

  fprintf (out, "%*sm_url_format: ", indent, "");
  switch (m_url_format)
    {
    case URL_FORMAT_NONE:
      fprintf (out, "none");
      break;
    case URL_FORMAT_ST:
      fprintf (out, "st");
      break;
    case URL_FORMAT_BEL:
      fprintf (out, "bel");
      break;
    default:
      gcc_unreachable ();
    }
  fprintf (out, "\n");

  fprintf (out, "%*sm_buffer:\n", indent, "");
  m_buffer->dump (out, indent + 2);
}

/* class pp_markup::context.  */

void
pp_markup::context::begin_quote ()
{
  gcc_assert (!m_quoted);
  gcc_assert (m_formatted_token_list);
  push_back_any_text ();
  m_formatted_token_list->push_back<pp_token_begin_quote> ();
  m_quoted = true;
}

void
pp_markup::context::end_quote ()
{
  /* Bail out if the quotes have already been ended, such as by
     printing a type emitting "TYPEDEF' {aka `TYPE'}".  */
  if (!m_quoted)
    return;
  gcc_assert (m_formatted_token_list);
  push_back_any_text ();
  m_formatted_token_list->push_back<pp_token_end_quote> ();
  m_quoted = false;
}

void
pp_markup::context::begin_highlight_color (const char *color_name)
{
  if (!pp_show_highlight_colors (&m_pp))
    return;

  push_back_any_text ();
  m_formatted_token_list->push_back <pp_token_begin_color>
    (label_text::borrow (color_name));
}

void
pp_markup::context::end_highlight_color ()
{
  if (!pp_show_highlight_colors (&m_pp))
    return;

  push_back_any_text ();
  m_formatted_token_list->push_back<pp_token_end_color> ();
}

void
pp_markup::context::push_back_any_text ()
{
  obstack *cur_obstack = m_buf.m_obstack;
  obstack_1grow (cur_obstack, '\0');
  m_formatted_token_list->push_back_text
    (label_text::borrow (XOBFINISH (cur_obstack,
				    const char *)));
}

void
pp_markup::comma_separated_quoted_strings::add_to_phase_2 (context &ctxt)
{
  for (unsigned i = 0; i < m_strings.length (); i++)
    {
      if (i > 0)
	pp_string (&ctxt.m_pp, ", ");
      ctxt.begin_quote ();
      pp_string (&ctxt.m_pp, m_strings[i]);
      ctxt.end_quote ();
    }
}

/* Color names for expressing "expected" vs "actual" values.  */
const char *const highlight_colors::expected = "highlight-a";
const char *const highlight_colors::actual   = "highlight-b";

/* Color names for expressing "LHS" vs "RHS" values in a binary operation.  */
const char *const highlight_colors::lhs = "highlight-a";
const char *const highlight_colors::rhs = "highlight-b";

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

  /* Verify %e.  */
  {
    pp_element_quoted_string foo ("foo");
    pp_element_quoted_string bar ("bar");
    ASSERT_PP_FORMAT_2 ("before `foo' `bar' after",
			"before %e %e after",
			&foo, &bar);
  }

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
  assert_pp_format (SELFTEST_LOCATION,
		    "foo: sec bar: 3360",
		    "foo: %3$.*2$s bar: %1$o",
		    1776, 3, "second");
  assert_pp_format (SELFTEST_LOCATION,
		    "foo: seco bar: 3360",
		    "foo: %2$.4s bar: %1$o",
		    1776, "second");
}

static void
test_merge_consecutive_text_tokens ()
{
  auto_obstack s;
  pp_token_list list (s);
  list.push_back_text (label_text::borrow ("hello"));
  list.push_back_text (label_text::borrow (" "));
  list.push_back_text (label_text::take (xstrdup ("world")));
  list.push_back_text (label_text::borrow ("!"));

  list.merge_consecutive_text_tokens ();
  // We expect a single text token, with concatenated text
  ASSERT_EQ (list.m_first, list.m_end);
  pp_token *tok = list.m_first;
  ASSERT_NE (tok, nullptr);
  ASSERT_EQ (tok->m_kind, pp_token::kind::text);
  ASSERT_STREQ (as_a <pp_token_text *> (tok)->m_value.get (), "hello world!");
}

/* Verify that we can create custom tokens that can be lowered
   in phase 3.  */

static void
test_custom_tokens_1 ()
{
  struct custom_token_adder : public pp_element
  {
  public:
    struct value : public pp_token_custom_data::value
    {
      value (custom_token_adder &adder)
      : m_adder (adder)
      {
	m_adder.m_num_living_values++;
      }
      value (const value &other)
      : m_adder (other.m_adder)
      {
	m_adder.m_num_living_values++;
      }
      value (value &&other)
      : m_adder (other.m_adder)
      {
	m_adder.m_num_living_values++;
      }
      value &operator= (const value &other) = delete;
      value &operator= (value &&other) = delete;
      ~value ()
      {
	m_adder.m_num_living_values--;
      }

      void dump (FILE *out) const final override
      {
	fprintf (out, "\"%s\"", m_adder.m_name);
      }

      bool as_standard_tokens (pp_token_list &out) final override
      {
	ASSERT_TRUE (m_adder.m_num_living_values > 0);
	out.push_back<pp_token_text> (label_text::borrow (m_adder.m_name));
	return true;
      }

      custom_token_adder &m_adder;
    };

    custom_token_adder (const char *name)
    : m_name (name),
      m_num_living_values (0)
    {
    }

    void add_to_phase_2 (pp_markup::context &ctxt) final override
    {
      auto val_ptr = make_unique<value> (*this);
      ctxt.m_formatted_token_list->push_back<pp_token_custom_data>
	(std::move (val_ptr));
    }

    const char *m_name;
    int m_num_living_values;
  };

  custom_token_adder e1 ("foo");
  custom_token_adder e2 ("bar");
  ASSERT_EQ (e1.m_num_living_values, 0);
  ASSERT_EQ (e2.m_num_living_values, 0);

  pretty_printer pp;
  pp_printf (&pp, "before %e middle %e after", &e1, &e2);

  /* Verify that instances were cleaned up.  */
  ASSERT_EQ (e1.m_num_living_values, 0);
  ASSERT_EQ (e2.m_num_living_values, 0);

  ASSERT_STREQ (pp_formatted_text (&pp),
		"before foo middle bar after");
}

/* Verify that we can create custom tokens that aren't lowered
   in phase 3, but instead are handled by a custom token_printer.
   Use this to verify the inputs seen by such token_printers.  */

static void
test_custom_tokens_2 ()
{
  struct custom_token_adder : public pp_element
  {
    struct value : public pp_token_custom_data::value
    {
    public:
      value (custom_token_adder &adder)
      : m_adder (adder)
      {
	m_adder.m_num_living_values++;
      }
      value (const value &other)
      : m_adder (other.m_adder)
      {
	m_adder.m_num_living_values++;
      }
      value (value &&other)
      : m_adder (other.m_adder)
      {
	m_adder.m_num_living_values++;
      }
      value &operator= (const value &other) = delete;
      value &operator= (value &&other) = delete;
      ~value ()
      {
	m_adder.m_num_living_values--;
      }

      void dump (FILE *out) const final override
      {
	fprintf (out, "\"%s\"", m_adder.m_name);
      }

      bool as_standard_tokens (pp_token_list &) final override
      {
	return false;
      }

      custom_token_adder &m_adder;
    };

    custom_token_adder (const char *name)
    : m_name (name),
      m_num_living_values (0)
    {
    }

    void add_to_phase_2 (pp_markup::context &ctxt) final override
    {
      auto val_ptr = make_unique<value> (*this);
      ctxt.m_formatted_token_list->push_back<pp_token_custom_data>
	(std::move (val_ptr));
    }

    const char *m_name;
    int m_num_living_values;
  };

  class custom_token_printer : public token_printer
  {
    void print_tokens (pretty_printer *pp,
		       const pp_token_list &tokens) final override
    {
      /* Verify that TOKENS has:
	 [TEXT("before "), CUSTOM("foo"), TEXT(" middle "), CUSTOM("bar"),
	  TEXT(" after")]  */
      pp_token *tok_0 = tokens.m_first;
      ASSERT_NE (tok_0, nullptr);
      ASSERT_EQ (tok_0->m_kind, pp_token::kind::text);
      ASSERT_STREQ (as_a<pp_token_text *> (tok_0)->m_value.get (),
		    "before ");

      pp_token *tok_1 = tok_0->m_next;
      ASSERT_NE (tok_1, nullptr);
      ASSERT_EQ (tok_1->m_prev, tok_0);
      ASSERT_EQ (tok_1->m_kind, pp_token::kind::custom_data);

      custom_token_adder::value *v1
	= static_cast <custom_token_adder::value *>
	(as_a<pp_token_custom_data *> (tok_1)->m_value.get ());
      ASSERT_STREQ (v1->m_adder.m_name, "foo");
      ASSERT_TRUE (v1->m_adder.m_num_living_values > 0);

      pp_token *tok_2 = tok_1->m_next;
      ASSERT_NE (tok_2, nullptr);
      ASSERT_EQ (tok_2->m_prev, tok_1);
      ASSERT_EQ (tok_2->m_kind, pp_token::kind::text);
      ASSERT_STREQ (as_a<pp_token_text *> (tok_2)->m_value.get (),
		    " middle ");

      pp_token *tok_3 = tok_2->m_next;
      ASSERT_NE (tok_3, nullptr);
      ASSERT_EQ (tok_3->m_prev, tok_2);
      ASSERT_EQ (tok_3->m_kind, pp_token::kind::custom_data);
      custom_token_adder::value *v3
	= static_cast <custom_token_adder::value *>
	(as_a<pp_token_custom_data *> (tok_3)->m_value.get ());
      ASSERT_STREQ (v3->m_adder.m_name, "bar");
      ASSERT_TRUE (v3->m_adder.m_num_living_values > 0);

      pp_token *tok_4 = tok_3->m_next;
      ASSERT_NE (tok_4, nullptr);
      ASSERT_EQ (tok_4->m_prev, tok_3);
      ASSERT_EQ (tok_4->m_kind, pp_token::kind::text);
      ASSERT_STREQ (as_a<pp_token_text *> (tok_4)->m_value.get (),
		    " after");
      ASSERT_EQ (tok_4->m_next, nullptr);

      /* Normally we'd loop over the tokens, printing them to PP
	 and handling the custom tokens.
	 Instead, print a message to PP to verify that we were called.  */
      pp_string (pp, "print_tokens was called");
    }
  };

  custom_token_adder e1 ("foo");
  custom_token_adder e2 ("bar");
  ASSERT_EQ (e1.m_num_living_values, 0);
  ASSERT_EQ (e2.m_num_living_values, 0);

  custom_token_printer tp;
  pretty_printer pp;
  pp.set_token_printer (&tp);
  pp_printf (&pp, "before %e middle %e after", &e1, &e2);

  /* Verify that instances were cleaned up.  */
  ASSERT_EQ (e1.m_num_living_values, 0);
  ASSERT_EQ (e2.m_num_living_values, 0);

  ASSERT_STREQ (pp_formatted_text (&pp),
		"print_tokens was called");
}

/* Helper subroutine for test_pp_format_stack.
   Call pp_format (phases 1 and 2), without calling phase 3.  */

static void
push_pp_format (pretty_printer *pp, const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  rich_location rich_loc (line_table, UNKNOWN_LOCATION);
  text_info ti (msg, &ap, 0, nullptr, &rich_loc);
  pp_format (pp, &ti);
  va_end (ap);
}

#define ASSERT_TEXT_TOKEN(TOKEN, EXPECTED_TEXT)		\
  SELFTEST_BEGIN_STMT						\
    ASSERT_NE ((TOKEN), nullptr);				\
    ASSERT_EQ ((TOKEN)->m_kind, pp_token::kind::text);		\
    ASSERT_STREQ						\
      (as_a <const pp_token_text *> (TOKEN)->m_value.get (),	\
       (EXPECTED_TEXT));					\
  SELFTEST_END_STMT


/* Verify that the stack of pp_formatted_chunks works as expected.  */

static void
test_pp_format_stack ()
{
  auto_fix_quotes fix_quotes;

  pretty_printer pp;
  push_pp_format (&pp, "unexpected foo: %i bar: %qs", 42, "test");
  push_pp_format (&pp, "In function: %qs", "test_fn");

  /* Expect the top of the stack to have:
     (gdb) call top->dump()
     0: [TEXT("In function: ")]
     1: [BEGIN_QUOTE, TEXT("test_fn"), END_QUOTE].  */

  pp_formatted_chunks *top = pp_buffer (&pp)->m_cur_formatted_chunks;
  ASSERT_NE (top, nullptr);
  ASSERT_TEXT_TOKEN (top->get_token_lists ()[0]->m_first, "In function: ");
  ASSERT_EQ (top->get_token_lists ()[1]->m_first->m_kind,
	     pp_token::kind::begin_quote);
  ASSERT_EQ (top->get_token_lists ()[2], nullptr);

  /* Expect an entry in the stack below it with:
     0: [TEXT("unexpected foo: ")]
     1: [TEXT("42")]
     2: [TEXT(" bar: ")]
     3: [BEGIN_QUOTE, TEXT("test"), END_QUOTE].  */
  pp_formatted_chunks *prev = top->get_prev ();
  ASSERT_NE (prev, nullptr);
  ASSERT_TEXT_TOKEN (prev->get_token_lists ()[0]->m_first, "unexpected foo: ");
  ASSERT_TEXT_TOKEN (prev->get_token_lists ()[1]->m_first, "42");
  ASSERT_TEXT_TOKEN (prev->get_token_lists ()[2]->m_first, " bar: ");
  ASSERT_EQ (prev->get_token_lists ()[3]->m_first->m_kind,
	     pp_token::kind::begin_quote);
  ASSERT_EQ (prev->get_token_lists ()[4], nullptr);

  ASSERT_EQ (prev->get_prev (), nullptr);

  /* Pop the top of the stack.  */
  pp_output_formatted_text (&pp);
  ASSERT_EQ (pp_buffer (&pp)->m_cur_formatted_chunks, prev);
  pp_newline (&pp);

  /* Pop the remaining entry from the stack.  */
  pp_output_formatted_text (&pp);
  ASSERT_EQ (pp_buffer (&pp)->m_cur_formatted_chunks, nullptr);

  ASSERT_STREQ (pp_formatted_text (&pp),
		"In function: `test_fn'\nunexpected foo: 42 bar: `test'");
}

/* Verify usage of pp_printf from within a pp_element's
   add_to_phase_2 vfunc.  */
static void
test_pp_printf_within_pp_element ()
{
  class kv_element : public pp_element
  {
  public:
    kv_element (const char *key, int value)
    : m_key (key), m_value (value)
    {
    }

    void add_to_phase_2 (pp_markup::context &ctxt) final override
    {
      /* We can't call pp_printf directly on ctxt.m_pp from within
	 formatting.  As a workaround, work with a clone of the pp.  */
      std::unique_ptr<pretty_printer> pp (ctxt.m_pp.clone ());
      pp_printf (pp.get (), "(%qs: %qi)", m_key, m_value);
      pp_string (&ctxt.m_pp, pp_formatted_text (pp.get ()));
    }

  private:
    const char *m_key;
    int m_value;
  };

  auto_fix_quotes fix_quotes;

  kv_element e1 ("foo", 42);
  kv_element e2 ("bar", 1066);
  ASSERT_PP_FORMAT_2 ("before (`foo': `42') (`bar': `1066') after",
		      "before %e %e after",
		      &e1, &e2);
  assert_pp_format_colored (SELFTEST_LOCATION,
			    ("before "
			     "(`\33[01m\33[Kfoo\33[m\33[K'"
			     ": "
			     "`\33[01m\33[K42\33[m\33[K')"
			     " "
			     "(`\33[01m\33[Kbar\33[m\33[K'"
			     ": "
			     "`\33[01m\33[K1066\33[m\33[K')"
			     " after"),
			    "before %e %e after",
			    &e1, &e2);
}

/* A subclass of pretty_printer for use by test_prefixes_and_wrapping.  */

class test_pretty_printer : public pretty_printer
{
 public:
  test_pretty_printer (enum diagnostic_prefixing_rule_t rule,
		       int max_line_length)
  {
    pp_set_prefix (this, xstrdup ("PREFIX: "));
    pp_prefixing_rule (this) = rule;
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

static void
test_urls ()
{
  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_NONE);
    pp_begin_url (&pp, "http://example.com");
    pp_string (&pp, "This is a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("This is a link",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_ST);
    pp_begin_url (&pp, "http://example.com");
    pp_string (&pp, "This is a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("\33]8;;http://example.com\33\\This is a link\33]8;;\33\\",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_BEL);
    pp_begin_url (&pp, "http://example.com");
    pp_string (&pp, "This is a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("\33]8;;http://example.com\aThis is a link\33]8;;\a",
		  pp_formatted_text (&pp));
  }
}

static void
test_urls_from_braces ()
{
  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_NONE);
    pp_printf (&pp, "before %{text%} after",
		    "http://example.com");
    ASSERT_STREQ ("before text after",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_ST);
    pp_printf (&pp, "before %{text%} after",
		    "http://example.com");
    ASSERT_STREQ ("before \33]8;;http://example.com\33\\text\33]8;;\33\\ after",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_BEL);
    pp_printf (&pp, "before %{text%} after",
		    "http://example.com");
    ASSERT_STREQ ("before \33]8;;http://example.com\atext\33]8;;\a after",
		  pp_formatted_text (&pp));
  }
}

/* Verify that we gracefully reject null URLs.  */

static void
test_null_urls ()
{
  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_NONE);
    pp_begin_url (&pp, nullptr);
    pp_string (&pp, "This isn't a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("This isn't a link",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_ST);
    pp_begin_url (&pp, nullptr);
    pp_string (&pp, "This isn't a link");
    pp_end_url (&pp);
    ASSERT_STREQ ("This isn't a link",
		  pp_formatted_text (&pp));
  }

  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_BEL);
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
  pp_format (pp, &text);
  pp_output_formatted_text (pp, urlifier);
  va_end (ap);
}

static void
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
      pp.set_url_format (URL_FORMAT_NONE);
      pp_printf_with_urlifier (&pp, &urlifier,
			       "foo %<-foption%> %<unrecognized%> bar");
      ASSERT_STREQ ("foo `-foption' `unrecognized' bar",
		    pp_formatted_text (&pp));
    }
    {
      pretty_printer pp;
      pp.set_url_format (URL_FORMAT_ST);
      pp_printf_with_urlifier (&pp, &urlifier,
			       "foo %<-foption%> %<unrecognized%> bar");
      ASSERT_STREQ
	("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\'"
	 " `unrecognized' bar",
	 pp_formatted_text (&pp));
    }
    {
      pretty_printer pp;
      pp.set_url_format (URL_FORMAT_BEL);
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
    pp.set_url_format (URL_FORMAT_ST);
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
    pp.set_url_format (URL_FORMAT_ST);
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
    pp.set_url_format (URL_FORMAT_ST);
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
    pp.set_url_format (URL_FORMAT_ST);
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
    pp.set_url_format (URL_FORMAT_ST);
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
    pp.set_url_format (URL_FORMAT_ST);
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
    pp.set_url_format (URL_FORMAT_ST);
    pp_printf_with_urlifier (&pp, &urlifier,
			     "foo %<-f%2$st%1$sn%> bar",
			     "io", "op");
    ASSERT_STREQ
      ("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' bar",
       pp_formatted_text (&pp));
  }

  /* Example of %e.  */
  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_ST);
    pp_element_quoted_string elem ("-foption");
    pp_printf_with_urlifier (&pp, &urlifier,
			     "foo %e bar",
			     &elem);
    ASSERT_STREQ
      ("foo `\33]8;;http://example.com\33\\-foption\33]8;;\33\\' bar",
       pp_formatted_text (&pp));
  }

  /* Test the example from pretty-print-format-impl.h.  */
  {
    pretty_printer pp;
    pp.set_url_format (URL_FORMAT_ST);
    pp_printf_with_urlifier (&pp, &urlifier,
	       "foo: %i, bar: %s, option: %qs",
	       42, "baz", "-foption");
    ASSERT_STREQ (pp_formatted_text (&pp),
		  "foo: 42, bar: baz, option:"
		  " `]8;;http://example.com\\-foption]8;;\\'");
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

/* Verify that class comma_separated_quoted_strings works as expected.  */

static void
test_comma_separated_quoted_strings ()
{
  auto_fix_quotes fix_quotes;

  auto_vec<const char *> none;
  pp_markup::comma_separated_quoted_strings e_none (none);

  auto_vec<const char *> one;
  one.safe_push ("one");
  pp_markup::comma_separated_quoted_strings e_one (one);

  auto_vec<const char *> many;
  many.safe_push ("0");
  many.safe_push ("1");
  many.safe_push ("2");
  pp_markup::comma_separated_quoted_strings e_many (many);

  ASSERT_PP_FORMAT_3 ("none: () one: (`one') many: (`0', `1', `2')",
		      "none: (%e) one: (%e) many: (%e)",
		      &e_none, &e_one, &e_many);
  assert_pp_format_colored (SELFTEST_LOCATION,
			    "one: (`[01m[Kone[m[K')",
			    "one: (%e)",
			    &e_one);
}

/* Run all of the selftests within this file.  */

void
pretty_print_cc_tests ()
{
  test_basic_printing ();
  test_pp_format ();
  test_merge_consecutive_text_tokens ();
  test_custom_tokens_1 ();
  test_custom_tokens_2 ();
  test_pp_format_stack ();
  test_pp_printf_within_pp_element ();
  test_prefixes_and_wrapping ();
  test_urls ();
  test_urls_from_braces ();
  test_null_urls ();
  test_urlification ();
  test_utf8 ();
  test_comma_separated_quoted_strings ();
}

} // namespace selftest

#endif /* CHECKING_P */
