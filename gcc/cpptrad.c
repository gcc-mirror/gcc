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

/* The replacement text of a function-like macro is stored as a
   contiguous sequence of aligned blocks.  Each block represents the
   portion of text from the start of the previous block (or the start
   of the macro replacement text in the case of the first block) to
   the next parameter, or the end of the replacement list if there
   are none left.

   Each block consists of an unsigned int, which is the length of text
   contained in the third part, an unsigned short, which is the
   one-based index of the argument that immediately follows that text,
   and the text itself.  The final block in the macro expansion is
   recognizable as it has an argument index of zero.  */

struct block
{
  unsigned int text_len;
  unsigned short arg_index;
  uchar text[1];
};

#define BLOCK_HEADER_LEN offsetof (struct block, text)
#define BLOCK_LEN(TEXT_LEN) CPP_ALIGN (BLOCK_HEADER_LEN + TEXT_LEN)

/* Lexing TODO: Handle -C, maybe -CC, and space in escaped newlines.
   Stop cpplex.c from recognizing comments and directives during its
   lexing pass.  Get rid of line_base usage - seems pointless?  Do we
   get escaped newline at EOF correct?  */

static const uchar *handle_newline PARAMS ((cpp_reader *, const uchar *));
static const uchar *skip_escaped_newlines PARAMS ((cpp_reader *,
						   const uchar *));
static const uchar *skip_whitespace PARAMS ((cpp_reader *, const uchar *));
static cpp_hashnode *lex_identifier PARAMS ((cpp_reader *, const uchar *));
static const uchar *skip_comment PARAMS ((cpp_reader *, const uchar *));
static void scan_out_logical_line PARAMS ((cpp_reader *pfile, cpp_macro *));
static void check_output_buffer PARAMS ((cpp_reader *, size_t));
static void restore_buff PARAMS ((cpp_reader *));
static void push_replacement_text PARAMS ((cpp_reader *, cpp_hashnode *));
static bool scan_parameters PARAMS ((cpp_reader *, cpp_macro *));
static void save_replacement_text PARAMS ((cpp_reader *, cpp_macro *,
					   unsigned int));
static unsigned int replacement_length PARAMS ((cpp_macro *));

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
  unsigned int c = 0, prevc = 0;
  const uchar *limit = RLIMIT (pfile->context);

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

/* Skip any horizontal whitespace and comments beginning at CUR,
   returning the following character.  */
static const uchar *
skip_whitespace (pfile, cur)
     cpp_reader *pfile;
     const uchar *cur;
{
  const uchar *tmp;

  for (;;)
    {
      while (is_nvspace (*cur) && *cur != 0)
	cur++;

      if (*cur == '\0' && cur != RLIMIT (pfile->context))
	continue;

      if (*cur == '\\')
	{
	  tmp = cur;
	  cur = skip_escaped_newlines (pfile, cur);
	  if (tmp != cur)
	    continue;
	}

      if (*cur == '/')
	{
	  tmp = skip_escaped_newlines (pfile, cur + 1);
	  if (*tmp == '*')
	    {
	      cur = skip_comment (pfile, tmp + 1);
	      continue;
	    }
	}

      break;
    }

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
  cpp_hashnode *result;

  do
    {
      do
	*out++ = *cur++;
      while (ISIDNUM (*cur));
      cur = skip_escaped_newlines (pfile, cur);
    }
  while (ISIDNUM (*cur));

  CUR (pfile->context) = cur;
  len = out - pfile->trad_out_cur;
  result = (cpp_hashnode *) ht_lookup (pfile->hash_table, pfile->trad_out_cur,
				       len, HT_ALLOC);
  pfile->trad_out_cur = out;
  return result;
}

/* Reads an identifier, returning its hashnode.  If the next token is
   not an identifier, returns NULL.  */
cpp_hashnode *
_cpp_lex_identifier_trad (pfile)
     cpp_reader *pfile;
{
  const uchar *cur = skip_whitespace (pfile, CUR (pfile->context));

  if (!ISIDST (*cur))
    {
      CUR (pfile->context) = cur;
      return NULL;
    }

  return lex_identifier (pfile, cur);
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

  CUR (pfile->context) = buffer->cur;
  RLIMIT (pfile->context) = buffer->rlimit;
  pfile->trad_out_cur = pfile->trad_out_base;
  scan_out_logical_line (pfile, NULL);
  buffer->cur = CUR (pfile->context);

  pfile->trad_line = pfile->line;
  pfile->line = first_line;
  _cpp_overlay_buffer (pfile, pfile->trad_out_base,
		       pfile->trad_out_cur - pfile->trad_out_base);
  return true;
}

/* Copies the next logical line in the current buffer to the output
   buffer.  The output is guaranteed to terminate with a NUL
   character.

   If MACRO is non-NULL, then we are scanning the replacement list of
   MACRO, and we call save_replacement_text every time we meet an
   argument.  */
static void
scan_out_logical_line (pfile, macro)
     cpp_reader *pfile;
     cpp_macro *macro;
{
  cpp_context *context;
  const uchar *cur;
  unsigned int c, quote = 0;
  uchar *out;

 new_context:
  context = pfile->context;
  cur = CUR (context);
  check_output_buffer (pfile, RLIMIT (context) - cur);
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
	  if (cur - 1 != RLIMIT (context))
	    break;

	  /* If this is a macro's expansion, pop it.  */
	  if (context->prev)
	    {
	      pfile->trad_out_cur = out - 1;
	      _cpp_pop_context (pfile);
	      goto new_context;
	    }

	  /* Premature end of file.  Fake a new line.  */
	  cur--;
	  if (!pfile->buffer->from_stage3)
	    cpp_error (pfile, DL_PEDWARN, "no newline at end of file");
	  pfile->line++;
	  goto finish_output;

	case '\r': case '\n':
	  cur = handle_newline (pfile, cur - 1);
	  out[-1] = '\0';
	finish_output:
	  CUR (context) = cur;
	  pfile->trad_out_cur = out - 1;
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
	  if (quote == 0 || macro)
	    {
	      cpp_hashnode *node;

	      pfile->trad_out_cur = --out;
	      node = lex_identifier (pfile, cur - 1);

	      if (node->type == NT_MACRO && !pfile->state.prevent_expansion)
		{
		  /* Remove the macro name from the output.  */
		  pfile->trad_out_cur = out;
		  push_replacement_text (pfile, node);
		  goto new_context;
		}
	      else if (macro && node->arg_index)
		{
		  /* Remove the macro name from the output.  */
		  pfile->trad_out_cur = out;
		  save_replacement_text (pfile, macro, node->arg_index);
		}

	      out = pfile->trad_out_cur;
	      cur = CUR (context);
	    }
	  break;

	default:
	  break;
	}
    }
}

/* Push a context holding the replacement text of the macro NODE on
   the context stack.  Doesn't yet handle special built-ins or
   function-like macros.  */
static void
push_replacement_text (pfile, node)
     cpp_reader *pfile;
     cpp_hashnode *node;
{
  cpp_macro *macro = node->value.macro;

  _cpp_push_text_context (pfile, node,
			  macro->exp.text,
			  macro->exp.text + macro->count);
}

/* Read and record the parameters, if any, of a function-like macro
   definition.  Destroys pfile->trad_out_cur.

   Returns true on success, false on failure (syntax error or a
   duplicate parameter).  On success, CUR (pfile->context) is just
   past the closing parenthesis.  */
static bool
scan_parameters (pfile, macro)
     cpp_reader *pfile;
     cpp_macro *macro;
{
  const uchar *cur = CUR (pfile->context) + 1;
  bool ok;

  for (;;)
    {
      cur = skip_whitespace (pfile, cur);

      if (ISIDST (*cur))
	{
	  ok = false;
	  if (_cpp_save_parameter (pfile, macro, lex_identifier (pfile, cur)))
	    break;
	  cur = skip_whitespace (pfile, CUR (pfile->context));
	  if (*cur == ',')
	    {
	      cur++;
	      continue;
	    }
	  ok = (*cur == ')');
	  break;
	}

      ok = (*cur == ')' && macro->paramc == 0);
      break;
    }

  CUR (pfile->context) = cur + (*cur == ')');

  return ok;
}

/* Calculate the length of the replacement text of MACRO.  */
static unsigned int
replacement_length (macro)
     cpp_macro *macro;
{
  unsigned int result = 0;
  const uchar *exp = macro->exp.text;

  for (;;)
    {
      struct block *block = (struct block *) exp;

      result += block->text_len;
      if (block->arg_index == 0)
	return result;
      exp += BLOCK_LEN (block->text_len);
    }
}

/* Save the text from pfile->trad_out_base to pfile->trad_out_cur as
   the replacement text for the current macro, followed by argument
   ARG_INDEX, with zero indicating the end of the replacement
   text.  */
static void
save_replacement_text (pfile, macro, arg_index)
     cpp_reader *pfile;
     cpp_macro *macro;
     unsigned int arg_index;
{
  size_t len = pfile->trad_out_cur - pfile->trad_out_base;
  uchar *exp;

  if (macro->paramc == 0)
    {
      /* Object-like and function-like macros without parameters
	 simply store their NUL-terminated replacement text.  */
      exp = _cpp_unaligned_alloc (pfile, len + 1);
      memcpy (exp, pfile->trad_out_base, len);
      exp[len] = '\0';
      macro->exp.text = exp;
      macro->count = len;
    }
  else
    {
      /* Store the text's length (unsigned int), the argument index
	 (unsigned short, base 1) and then the text.  */
      size_t blen = BLOCK_LEN (len);
      struct block *block;

      if (macro->count + blen > BUFF_ROOM (pfile->a_buff))
	_cpp_extend_buff (pfile, &pfile->a_buff, macro->count + blen);

      exp = BUFF_FRONT (pfile->a_buff);
      block = (struct block *) (exp + macro->count);
      macro->exp.text = exp;

      /* Write out the block information.  */
      block->text_len = len;
      block->arg_index = arg_index;
      memcpy (block->text, pfile->trad_out_base, len);

      /* Lex the rest into the start of the output buffer.  */
      pfile->trad_out_cur = pfile->trad_out_base;

      /* If this is the end of the macro, count up the bytes of text
	 in the replacement list, excluding the parameter names, and
	 save this in macro->count, else store the total bytes in the
	 replacement text so far (including block headers).  */
      if (arg_index == 0)
	macro->count = replacement_length (macro);
      else
	macro->count += blen;
    }
}

/* Analyze and save the replacement text of a macro.  Returns true on
   success.  */
bool
_cpp_create_trad_definition (pfile, macro)
     cpp_reader *pfile;
     cpp_macro *macro;
{
  const uchar *cur;
  uchar *limit;

  /* Skip leading whitespace now.  */
  CUR (pfile->context) = skip_whitespace (pfile, CUR (pfile->context));

  /* Is this a function-like macro?  */
  if (* CUR (pfile->context) == '(')
    {
      /* Setting macro to NULL indicates an error occurred.  */
      if (!scan_parameters (pfile, macro))
	macro = NULL;
      else
	{
	  /* Success.  Commit the parameter array.  */
	  macro->params = (cpp_hashnode **) BUFF_FRONT (pfile->a_buff);
	  BUFF_FRONT (pfile->a_buff) = (uchar *) &macro->params[macro->paramc];
	  macro->fun_like = 1;
	}

      CUR (pfile->context) = skip_whitespace (pfile, CUR (pfile->context));
    }

  pfile->trad_out_cur = pfile->trad_out_base;
  pfile->state.prevent_expansion++;
  scan_out_logical_line (pfile, macro);
  pfile->state.prevent_expansion--;

  if (!macro)
    return false;

  /* Skip trailing white space.  */
  cur = pfile->trad_out_base;
  limit = pfile->trad_out_cur;
  while (limit > cur && is_space (limit[-1]))
    limit--;
  pfile->trad_out_cur = limit;
  save_replacement_text (pfile, macro, 0);

  return true;
}

/* Prepare to be able to scan the current buffer.  */
void
_cpp_set_trad_context (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  cpp_context *context = pfile->context;

  if (pfile->context->prev)
    abort ();

  pfile->trad_out_cur = pfile->trad_out_base;
  CUR (context) = buffer->cur;
  RLIMIT (context) = buffer->rlimit;
  check_output_buffer (pfile, RLIMIT (context) - CUR (context));
}
