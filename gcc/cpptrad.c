/* CPP Library - traditional lexical analysis and macro expansion.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Neil Booth, May 2002

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "cpphash.h"

/* Lexing TODO: Handle -C, maybe -CC, and space in escaped newlines.
   Stop cpplex.c from recognizing comments and directives during its
   lexing pass.  */

static const uchar *handle_newline PARAMS ((cpp_reader *, const uchar *));
static const uchar *skip_escaped_newlines PARAMS ((cpp_reader *,
						   const uchar *));
static cpp_hashnode *lex_identifier PARAMS ((cpp_reader *, const uchar *));
static const uchar *skip_comment PARAMS ((cpp_reader *, const uchar *));
static void scan_out_logical_line PARAMS ((cpp_reader *pfile));
static void check_output_buffer PARAMS ((cpp_reader *, size_t));
static void restore_buff PARAMS ((cpp_reader *));

/* Ensures we have N bytes' space in the output buffer, and
   reallocates it if not.  */
static void
check_output_buffer (pfile, n)
     cpp_reader *pfile;
     size_t n;
{
  if (n > (size_t) (pfile->trad_out_limit - pfile->trad_out_cur))
    {
      size_t size = pfile->trad_out_cur - pfile->trad_out_base;
      size_t new_size = (size + n) * 3 / 2;

      pfile->trad_out_base
	= (uchar *) xrealloc (pfile->trad_out_base, new_size);
      pfile->trad_out_limit = pfile->trad_out_base + new_size;
      pfile->trad_out_cur = pfile->trad_out_base + size;
    }
}

/* To be called whenever a newline character is encountered in the
   input file, at CUR.  Handles DOS, MAC and Unix ends of line, and
   returns the character after the newline sequence.  */
static const uchar *
handle_newline (pfile, cur)
     cpp_reader *pfile;
     const uchar *cur;
{
  pfile->line++;
  if (cur[0] + cur[1] == '\r' + '\n')
    cur++;
  pfile->buffer->line_base = cur + 1;
  return cur + 1;
}

/* CUR points to any character in the buffer, not necessarily a
   backslash.  Advances CUR until all escaped newlines are skipped,
   and returns the new position.  */
static const uchar *
skip_escaped_newlines (pfile, cur)
     cpp_reader *pfile;
     const uchar *cur;
{
  while (*cur == '\\' && is_vspace (cur[1]))
    cur = handle_newline (pfile, cur + 1);

  return cur;
}

/* CUR points to the character after the asterisk introducing a
   comment.  Returns the position after the comment.  */
static const uchar *
skip_comment (pfile, cur)
     cpp_reader *pfile;
     const uchar *cur;
{
  unsigned int from_line = pfile->line;
  unsigned int c = 0, prevc;
  const uchar *limit = pfile->buffer->rlimit;

  while (cur < limit)
    {
      prevc = c;
      c = *cur++;

      if (c == '/')
	{
	  if (prevc == '*')
	    break;
	  if (*cur == '*' && cur[1] != '/'
	      && CPP_OPTION (pfile, warn_comments))
	    cpp_error_with_line (pfile, DL_WARNING, pfile->line, 0,
				 "\"/*\" within comment");
	}
      else if (is_vspace (c))
	cur = handle_newline (pfile, cur - 1);
    }

  if (c != '/' || prevc != '*')
    cpp_error_with_line (pfile, DL_ERROR, from_line, 0,
			 "unterminated comment");

  return cur;
}

/* Lexes and outputs an identifier starting at CUR, which is assumed
   to point to a valid first character of an identifier.  Returns
   the hashnode, and updates trad_out_cur.  */
static cpp_hashnode *
lex_identifier (pfile, cur)
     cpp_reader *pfile;
     const uchar *cur;
{
  size_t len;
  uchar *out = pfile->trad_out_cur;

  do
    {
      do
	*out++ = *cur++;
      while (ISIDNUM (*cur));
      cur = skip_escaped_newlines (pfile, cur);
    }
  while (ISIDNUM (*cur));

  pfile->buffer->cur = cur;
  len = out - pfile->trad_out_cur;
  pfile->trad_out_cur = out;
  return (cpp_hashnode *) ht_lookup (pfile->hash_table, pfile->trad_out_cur,
				     len, HT_ALLOC);
}

/* Overlays the true file buffer temporarily with text of length LEN
   starting at START.  The true buffer is restored upon calling
   restore_buff().  */
void
_cpp_overlay_buffer (pfile, start, len)
     cpp_reader *pfile;
     const uchar *start;
     size_t len;
{
  cpp_buffer *buffer = pfile->buffer;

  buffer->saved_cur = buffer->cur;
  buffer->saved_rlimit = buffer->rlimit;
  buffer->saved_line_base = buffer->line_base;

  buffer->cur = start;
  buffer->line_base = start;
  buffer->rlimit = start + len;
}

/* Restores a buffer overlaid by _cpp_overlay_buffer().  */
static void
restore_buff (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;

  buffer->cur = buffer->saved_cur;
  buffer->rlimit = buffer->saved_rlimit;
  buffer->line_base = buffer->saved_line_base;
}

/* Reads a logical line into the output buffer.  Returns TRUE if there
   is more text left in the buffer.  */
bool
_cpp_read_logical_line_trad (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer;
  unsigned int first_line;

  restore_buff (pfile);

  first_line = pfile->line = pfile->trad_line;

  buffer = pfile->buffer;
  if (buffer->cur == buffer->rlimit)
    {
      bool stop = true;

      /* Don't pop the last buffer.  */
      if (buffer->prev)
	{
	  stop = buffer->return_at_eof;
	  _cpp_pop_buffer (pfile);
	}

      if (stop)
	return false;
    }

  pfile->trad_out_cur = pfile->trad_out_base;
  scan_out_logical_line (pfile);
  pfile->trad_line = pfile->line;
  pfile->line = first_line;
  _cpp_overlay_buffer (pfile, pfile->trad_out_base,
		       pfile->trad_out_cur - pfile->trad_out_base);
  return true;
}

/* Copies the next logical line in the current buffer to the output
   buffer.  The output is guaranteed to terminate with a NUL
   character.  */
static void
scan_out_logical_line (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  const uchar *cur = buffer->cur;
  unsigned int c, quote = 0;
  uchar *out;

  check_output_buffer (pfile, buffer->rlimit - cur);
  out = pfile->trad_out_cur;

  for (;;)
    {
      c = *cur++;
      *out++ = c;

      /* There are only a few entities we need to catch: comments,
	 identifiers, newlines, escaped newlines, # and '\0'.  */
      switch (c)
	{
	case '\0':
	  if (cur - 1 != buffer->rlimit)
	    break;
	  cur--;
	  if (!buffer->from_stage3)
	    cpp_error (pfile, DL_PEDWARN, "no newline at end of file");
	  pfile->line++;
	  if (0)
	    {
	    case '\r': case '\n':
	      cur = handle_newline (pfile, cur - 1);
	    }
	  out[-1] = '\n';
	  out[0] = '\0';
	  buffer->cur = cur;
	  pfile->trad_out_cur = out;
	  return;

	case '"':
	case '\'':
	  if (c == quote)
	    quote = 0;
	  else if (!quote)
	    quote = c;
	  break;

	case '\\':
	  if (is_vspace (*cur))
	    out--, cur = skip_escaped_newlines (pfile, cur - 1);
	  else
	    {
	      /* Skip escaped quotes here, it's easier than above, but
		 take care to first skip escaped newlines.  */
	      cur = skip_escaped_newlines (pfile, cur);
	      if (*cur == '\\' || *cur == '"' || *cur == '\'')
		*out++ = *cur++;
	    }
	  break;

	case '/':
	  /* Traditional CPP does not recognize comments within
	     literals.  */
	  if (!quote)
	    {
	      cur = skip_escaped_newlines (pfile, cur);
	      if (*cur == '*')
		out--, cur = skip_comment (pfile, cur + 1);
	    }
	  break;

	case '_':
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
	case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
	case 's': case 't': case 'u': case 'v': case 'w': case 'x':
	case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
	case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
	case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
	case 'Y': case 'Z':
	  {
	    cpp_hashnode *node;

	    pfile->trad_out_cur = --out;
	    node = lex_identifier (pfile, cur - 1);
	    out = pfile->trad_out_cur;
	    cur = buffer->cur;
	  }
	  break;

	default:
	  break;
	}
    }
}
