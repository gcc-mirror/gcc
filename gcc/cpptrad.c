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
   contiguous sequence of aligned blocks, each representing the text
   between subsequent parameters in that text.

   Each block comprises the length of text contained therein, the
   one-based index of the argument that immediately follows that text,
   and the text itself.  The final block in the macro expansion is
   easily recognizable as it has an argument index of zero.  */

struct block
{
  unsigned int text_len;
  unsigned short arg_index;
  uchar text[1];
};

#define BLOCK_HEADER_LEN offsetof (struct block, text)
#define BLOCK_LEN(TEXT_LEN) CPP_ALIGN (BLOCK_HEADER_LEN + TEXT_LEN)

/* Structure holding information about a function-like macro
   invocation.  */
struct fun_macro
{
  /* Memory buffer holding the trad_arg array.  */
  _cpp_buff *buff;

  /* An array of size the number of macro parameters + 1, containing
     the offsets of the start of each macro argument in the output
     buffer.  The argument continues until the character before the
     start of the next one.  */
  size_t *args;

  /* The hashnode of the macro.  */
  cpp_hashnode *node;

  /* The offset of the macro name in the output buffer.  */
  size_t offset;

  /* Zero-based index of argument being currently lexed.  */
  unsigned int argc;
};

/* Lexing TODO: Maybe handle space in escaped newlines.  Stop cpplex.c
   from recognizing comments and directives during its lexing pass.  */

static const uchar *handle_newline PARAMS ((cpp_reader *, const uchar *));
static const uchar *skip_escaped_newlines PARAMS ((cpp_reader *,
						   const uchar *));
static const uchar *skip_whitespace PARAMS ((cpp_reader *, const uchar *,
					     int));
static cpp_hashnode *lex_identifier PARAMS ((cpp_reader *, const uchar *));
static const uchar *copy_comment PARAMS ((cpp_reader *, const uchar *, int));
static void scan_out_logical_line PARAMS ((cpp_reader *pfile, cpp_macro *));
static void check_output_buffer PARAMS ((cpp_reader *, size_t));
static void push_replacement_text PARAMS ((cpp_reader *, cpp_hashnode *));
static bool scan_parameters PARAMS ((cpp_reader *, cpp_macro *));
static bool recursive_macro PARAMS ((cpp_reader *, cpp_hashnode *));
static void save_replacement_text PARAMS ((cpp_reader *, cpp_macro *,
					   unsigned int));
static void maybe_start_funlike PARAMS ((cpp_reader *, cpp_hashnode *,
					 const uchar *, struct fun_macro *));
static void save_argument PARAMS ((struct fun_macro *, size_t));
static void replace_args_and_push PARAMS ((cpp_reader *, struct fun_macro *));
static size_t canonicalize_text PARAMS ((uchar *, const uchar *, size_t,
					 uchar *));

/* Ensures we have N bytes' space in the output buffer, and
   reallocates it if not.  */
static void
check_output_buffer (pfile, n)
     cpp_reader *pfile;
     size_t n;
{
  /* We might need two bytes to terminate an unterminated comment, and
     one more to terminate the line with a NUL.  */
  n += 2 + 1;

  if (n > (size_t) (pfile->out.limit - pfile->out.cur))
    {
      size_t size = pfile->out.cur - pfile->out.base;
      size_t new_size = (size + n) * 3 / 2;

      pfile->out.base
	= (uchar *) xrealloc (pfile->out.base, new_size);
      pfile->out.limit = pfile->out.base + new_size;
      pfile->out.cur = pfile->out.base + size;
    }
}

/* To be called whenever a newline character is encountered in the
   input file, at CUR.  Handles DOS, Mac and Unix ends of line, and
   increments pfile->line.

   Returns a pointer the character after the newline sequence.  */
static const uchar *
handle_newline (pfile, cur)
     cpp_reader *pfile;
     const uchar *cur;
{
  pfile->line++;
  if (cur[0] + cur[1] == '\r' + '\n')
    cur++;
  return cur + 1;
}

/* CUR points to any character in the buffer, not necessarily a
   backslash.  Advances CUR until all escaped newlines are skipped,
   and returns the new position.

   Warns if a file buffer ends in an escaped newline.  */
static const uchar *
skip_escaped_newlines (pfile, cur)
     cpp_reader *pfile;
     const uchar *cur;
{
  const uchar *orig_cur = cur;

  while (*cur == '\\' && is_vspace (cur[1]))
    cur = handle_newline (pfile, cur + 1);

  if (cur != orig_cur && cur == RLIMIT (pfile->context) && pfile->buffer->inc)
    cpp_error (pfile, DL_PEDWARN, "backslash-newline at end of file");

  return cur;
}

/* CUR points to the asterisk introducing a comment in the input
   buffer.  IN_DEFINE is true if we are in the replacement text
   of a macro.

   The asterisk and following comment is copied to the buffer pointed
   to by pfile->out.cur, which must be of sufficient size.
   Unterminated comments are diagnosed, and correctly terminated in
   the output.  pfile->out.cur is updated depending upon IN_DEFINE,
   -C, -CC and pfile->state.in_directive.

   Returns a pointer to the first character after the comment in the
   input buffer.  */
static const uchar *
copy_comment (pfile, cur, in_define)
     cpp_reader *pfile;
     const uchar *cur;
     int in_define;
{
  unsigned int from_line = pfile->line;
  const uchar *limit = RLIMIT (pfile->context);
  uchar *out = pfile->out.cur;

  do
    {
      unsigned int c = *cur++;
      *out++ = c;

      if (c == '/')
	{
	  /* An immediate slash does not terminate the comment.  */
	  if (out[-2] == '*' && out - 2 > pfile->out.cur)
	    goto done;

	  if (*cur == '*' && cur[1] != '/'
	      && CPP_OPTION (pfile, warn_comments))
	    cpp_error_with_line (pfile, DL_WARNING, pfile->line, 0,
				 "\"/*\" within comment");
	}
      else if (is_vspace (c))
	{
	  cur = handle_newline (pfile, cur - 1);
	  /* Canonicalize newline sequences and skip escaped ones.  */
	  if (out[-2] == '\\')
	    out -= 2;
	  else
	    out[-1] = '\n';
	}
    }
  while (cur < limit);

  cpp_error_with_line (pfile, DL_ERROR, from_line, 0, "unterminated comment");
  *out++ = '*';
  *out++ = '/';

 done:
  /* Comments in directives become spaces so that tokens are properly
     separated when the ISO preprocessor re-lexes the line.  The
     exception is #define.  */
  if (pfile->state.in_directive)
    {
      if (in_define)
	{
	  if (CPP_OPTION (pfile, discard_comments_in_macro_exp))
	    pfile->out.cur--;
	  else
	    pfile->out.cur = out;
	}
      else
	pfile->out.cur[-1] = ' ';
    }
  else if (CPP_OPTION (pfile, discard_comments))
    pfile->out.cur--;
  else
    pfile->out.cur = out;

  return cur;
}

/* CUR points to any character in the input buffer.  Skips over all
   contiguous horizontal white space and NULs, including comments if
   SKIP_COMMENTS, until reaching the first non-horizontal-whitespace
   character or the end of the current context.  Escaped newlines are
   removed.

   The whitespace is copied verbatim to the output buffer, except that
   comments are handled as described in copy_comment().
   pfile->out.cur is updated.

   Returns a pointer to the first character after the whitespace in
   the input buffer.  */
static const uchar *
skip_whitespace (pfile, cur, skip_comments)
     cpp_reader *pfile;
     const uchar *cur;
     int skip_comments;
{
  uchar *out = pfile->out.cur;

  for (;;)
    {
      unsigned int c = *cur++;
      *out++ = c;

      if (is_nvspace (c) && c)
	continue;

      if (!c && cur != RLIMIT (pfile->context))
	continue;

      if (*cur == '/' && skip_comments)
	{
	  const uchar *tmp = skip_escaped_newlines (pfile, cur);
	  if (*tmp == '*')
	    {
	      pfile->out.cur = out;
	      cur = copy_comment (pfile, tmp, false /* in_define */);
	      out = pfile->out.cur;
	      continue;
	    }
	}

      out--;
      if (c == '\\' && is_vspace (*cur))
	{
	  cur = skip_escaped_newlines (pfile, cur);
	  continue;
	}

      break;
    }

  pfile->out.cur = out;
  return cur - 1;
}

/* Lexes and outputs an identifier starting at CUR, which is assumed
   to point to a valid first character of an identifier.  Returns
   the hashnode, and updates out.cur.  */
static cpp_hashnode *
lex_identifier (pfile, cur)
     cpp_reader *pfile;
     const uchar *cur;
{
  size_t len;
  uchar *out = pfile->out.cur;
  cpp_hashnode *result;

  do
    {
      do
	*out++ = *cur++;
      while (is_numchar (*cur));
      cur = skip_escaped_newlines (pfile, cur);
    }
  while (is_numchar (*cur));

  CUR (pfile->context) = cur;
  len = out - pfile->out.cur;
  result = (cpp_hashnode *) ht_lookup (pfile->hash_table, pfile->out.cur,
				       len, HT_ALLOC);
  pfile->out.cur = out;
  return result;
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

  buffer->cur = start;
  buffer->rlimit = start + len;

  pfile->saved_line = pfile->line;
}

/* Restores a buffer overlaid by _cpp_overlay_buffer().  */
void
_cpp_remove_overlay (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;

  buffer->cur = buffer->saved_cur;
  buffer->rlimit = buffer->saved_rlimit;

  pfile->line = pfile->saved_line;
}

/* Reads a logical line into the output buffer.  Returns TRUE if there
   is more text left in the buffer.  */
bool
_cpp_read_logical_line_trad (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;

  do
    {
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
      scan_out_logical_line (pfile, NULL);
      buffer->cur = CUR (pfile->context);
    }
  while (pfile->state.skipping);

  return true;
}

/* Set up state for finding the opening '(' of a function-like
   macro.  */
static void
maybe_start_funlike (pfile, node, start, macro)
     cpp_reader *pfile;
     cpp_hashnode *node;
     const uchar *start;
     struct fun_macro *macro;
{
  unsigned int n = node->value.macro->paramc + 1;

  if (macro->buff)
    _cpp_release_buff (pfile, macro->buff);
  macro->buff = _cpp_get_buff (pfile, n * sizeof (size_t));
  macro->args = (size_t *) BUFF_FRONT (macro->buff);
  macro->node = node;
  macro->offset = start - pfile->out.base;
  macro->argc = 0;

  pfile->state.parsing_args = 1;
}

/* Save the OFFSET of the start of the next argument to MACRO.  */
static void
save_argument (macro, offset)
     struct fun_macro *macro;
     size_t offset;
{
  macro->argc++;
  if (macro->argc <= macro->node->value.macro->paramc)
    macro->args[macro->argc] = offset;
}

/* Copies the next logical line in the current buffer to the output
   buffer.  The output is guaranteed to terminate with a NUL
   character.

   If MACRO is non-NULL, then we are scanning the replacement list of
   MACRO, and we call save_replacement_text() every time we meet an
   argument.  */
static void
scan_out_logical_line (pfile, macro)
     cpp_reader *pfile;
     cpp_macro *macro;
{
  cpp_context *context;
  const uchar *cur;
  unsigned int c, paren_depth = 0, quote = 0;
  uchar *out;
  struct fun_macro fmacro;

  fmacro.buff = NULL;

 start_logical_line:
  pfile->out.cur = pfile->out.base;
  pfile->out.first_line = pfile->line;
 new_context:
  context = pfile->context;
  cur = CUR (context);
  check_output_buffer (pfile, RLIMIT (context) - cur);
  out = pfile->out.cur;

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
	      pfile->out.cur = out - 1;
	      _cpp_pop_context (pfile);
	      goto new_context;
	    }

	  /* Premature end of file.  Fake a new line.  */
	  cur--;
	  if (!pfile->buffer->from_stage3)
	    cpp_error (pfile, DL_PEDWARN, "no newline at end of file");
	  if (pfile->state.parsing_args == 2)
	    cpp_error (pfile, DL_ERROR,
		       "unterminated argument list invoking macro \"%s\"",
		       NODE_NAME (fmacro.node));
	  pfile->line++;
	  goto done;

	case '\r': case '\n':
	  cur = handle_newline (pfile, cur - 1);
	  if (pfile->state.parsing_args == 2)
	    {
	      /* Newlines in arguments become a space.  */
	      out[-1] = ' ';
	      continue;
	    }
	  goto done;

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
		{
		  pfile->out.cur = out;
		  cur = copy_comment (pfile, cur, macro != 0);
		  out = pfile->out.cur;
		}
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

	      pfile->out.cur = --out;
	      node = lex_identifier (pfile, cur - 1);

	      if (node->type == NT_MACRO
		  && !pfile->state.skipping
		  && pfile->state.parsing_args != 2
		  && !pfile->state.prevent_expansion
		  && !recursive_macro (pfile, node))
		{
		  if (node->value.macro->fun_like)
		    maybe_start_funlike (pfile, node, out, &fmacro);
		  else
		    {
		      /* Remove the object-like macro's name from the
			 output, and push its replacement text.  */
		      pfile->out.cur = out;
		      push_replacement_text (pfile, node);
		      goto new_context;
		    }
		}
	      else if (macro && node->arg_index)
		{
		  /* Found a parameter in the replacement text of a
		     #define.  Remove its name from the output.  */
		  pfile->out.cur = out;
		  save_replacement_text (pfile, macro, node->arg_index);
		}

	      out = pfile->out.cur;
	      cur = CUR (context);
	    }
	  break;

	case '(':
	  if (quote == 0)
	    {
	      paren_depth++;
	      if (pfile->state.parsing_args == 1)
		{
		  const uchar *p = pfile->out.base + fmacro.offset;

		  /* Invoke a prior function-like macro if there is only
		     white space in-between.  */
		  while (is_numchar (*p))
		    p++;
		  while (is_space (*p))
		    p++;

		  if (p == out - 1)
		    {
		      pfile->state.parsing_args = 2;
		      paren_depth = 1;
		      out = pfile->out.base + fmacro.offset;
		      fmacro.args[0] = fmacro.offset;
		    }
		  else
		    pfile->state.parsing_args = 0;
		}
	    }
	  break;

	case ',':
	  if (quote == 0 && pfile->state.parsing_args == 2 && paren_depth == 1)
	    save_argument (&fmacro, out - pfile->out.base);
	  break;

	case ')':
	  if (quote == 0)
	    {
	      paren_depth--;
	      if (pfile->state.parsing_args == 2 && paren_depth == 0)
		{
		  cpp_macro *m = fmacro.node->value.macro;

		  pfile->state.parsing_args = 0;
		  save_argument (&fmacro, out - pfile->out.base);

		  /* A single zero-length argument is no argument.  */
		  if (fmacro.argc == 1
		      && m->paramc == 0
		      && out == pfile->out.base + 1)
		    fmacro.argc = 0;

		  if (_cpp_arguments_ok (pfile, m, fmacro.node, fmacro.argc))
		    {
		      /* Remove the macro's invocation from the
			 output, and push its replacement text.  */
		      pfile->out.cur = (pfile->out.base
					     + fmacro.offset);
		      CUR (context) = cur;
		      replace_args_and_push (pfile, &fmacro);
		      goto new_context;
		    }
		}
	    }
	  break;

	case '#':
	  /* At start of a line it's a directive.  */
	  if (out - 1 == pfile->out.base && !pfile->state.in_directive)
	    {
	      /* This is a kludge.  We want to have the ISO
		 preprocessor lex the next token.  */
	      pfile->buffer->cur = cur;
	      if (_cpp_handle_directive (pfile, false /* indented */))
		goto start_logical_line;
	    }
	  break;

	default:
	  break;
	}
    }

 done:
  out[-1] = '\0';
  CUR (context) = cur;
  pfile->out.cur = out - 1;
  if (fmacro.buff)
    _cpp_release_buff (pfile, fmacro.buff);
}

/* Push a context holding the replacement text of the macro NODE on
   the context stack.  NODE is either object-like, or a function-like
   macro with no arguments.  */
static void
push_replacement_text (pfile, node)
     cpp_reader *pfile;
     cpp_hashnode *node;
{
  cpp_macro *macro = node->value.macro;

  _cpp_push_text_context (pfile, node, macro->exp.text, macro->count);
}

/* Returns TRUE if traditional macro recursion is detected.  */
static bool
recursive_macro (pfile, node)
     cpp_reader *pfile;
     cpp_hashnode *node;
{
  bool recursing = node->flags & NODE_DISABLED;

  /* Object-like macros that are already expanding are necessarily
     recursive.

     However, it is possible to have traditional function-like macros
     that are not infinitely recursive but recurse to any given depth.
     Further, it is easy to construct examples that get ever longer
     until the point they stop recursing.  So there is no easy way to
     detect true recursion; instead we assume any expansion more than
     20 deep since the first invocation of this macro must be
     recursing.  */
  if (recursing && node->value.macro->fun_like)
    {
      size_t depth = 0;
      cpp_context *context = pfile->context;

      do
	{
	  depth++;
	  if (context->macro == node && depth > 20)
	    break;
	  context = context->prev;
	}
      while (context);
      recursing = context != NULL;
    }

  if (recursing)
    cpp_error (pfile, DL_ERROR,
	       "detected recursion whilst expanding macro \"%s\"",
	       NODE_NAME (node));

  return recursing;
}

/* Push a context holding the replacement text of the macro NODE on
   the context stack.  NODE is either object-like, or a function-like
   macro with no arguments.  */
static void
replace_args_and_push (pfile, fmacro)
     cpp_reader *pfile;
     struct fun_macro *fmacro;
{
  cpp_macro *macro = fmacro->node->value.macro;

  if (macro->paramc == 0)
    push_replacement_text (pfile, fmacro->node);
  else
    {
      const uchar *exp;
      uchar *p;
      _cpp_buff *buff;
      size_t len = 0;

      /* Calculate the length of the argument-replaced text.  */
      for (exp = macro->exp.text;;)
	{
	  struct block *b = (struct block *) exp;

	  len += b->text_len;
	  if (b->arg_index == 0)
	    break;
	  len += (fmacro->args[b->arg_index]
		  - fmacro->args[b->arg_index - 1] - 1);
	  exp += BLOCK_LEN (b->text_len);
	}

      /* Allocate room for the expansion plus NUL.  */
      buff = _cpp_get_buff (pfile, len + 1);

      /* Copy the expansion and replace arguments.  */
      p = BUFF_FRONT (buff);
      for (exp = macro->exp.text;;)
	{
	  struct block *b = (struct block *) exp;
	  size_t arglen;

	  memcpy (p, b->text, b->text_len);
	  p += b->text_len;
	  if (b->arg_index == 0)
	    break;
	  arglen = (fmacro->args[b->arg_index]
		    - fmacro->args[b->arg_index - 1] - 1);
	  memcpy (p, pfile->out.base + fmacro->args[b->arg_index - 1],
		  arglen);
	  p += arglen;
	  exp += BLOCK_LEN (b->text_len);
	}

      /* NUL-terminate.  */
      *p = '\0';
      _cpp_push_text_context (pfile, fmacro->node, BUFF_FRONT (buff), len);

      /* So we free buffer allocation when macro is left.  */
      pfile->context->buff = buff;
    }
}

/* Read and record the parameters, if any, of a function-like macro
   definition.  Destroys pfile->out.cur.

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
      cur = skip_whitespace (pfile, cur, true /* skip_comments */);

      if (is_idstart (*cur))
	{
	  ok = false;
	  if (_cpp_save_parameter (pfile, macro, lex_identifier (pfile, cur)))
	    break;
	  cur = skip_whitespace (pfile, CUR (pfile->context),
				 true /* skip_comments */);
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

/* Save the text from pfile->out.base to pfile->out.cur as
   the replacement text for the current macro, followed by argument
   ARG_INDEX, with zero indicating the end of the replacement
   text.  */
static void
save_replacement_text (pfile, macro, arg_index)
     cpp_reader *pfile;
     cpp_macro *macro;
     unsigned int arg_index;
{
  size_t len = pfile->out.cur - pfile->out.base;
  uchar *exp;

  if (macro->paramc == 0)
    {
      /* Object-like and function-like macros without parameters
	 simply store their NUL-terminated replacement text.  */
      exp = _cpp_unaligned_alloc (pfile, len + 1);
      memcpy (exp, pfile->out.base, len);
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
      memcpy (block->text, pfile->out.base, len);

      /* Lex the rest into the start of the output buffer.  */
      pfile->out.cur = pfile->out.base;

      macro->count += blen;

      /* If we've finished, commit the memory.  */
      if (arg_index == 0)
	BUFF_FRONT (pfile->a_buff) += macro->count;
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

  CUR (pfile->context) = pfile->buffer->cur;

  /* Is this a function-like macro?  */
  if (* CUR (pfile->context) == '(')
    {
      /* Setting macro to NULL indicates an error occurred, and
	 prevents unnecessary work in scan_out_logical_line.  */
      if (!scan_parameters (pfile, macro))
	macro = NULL;
      else
	{
	  /* Success.  Commit the parameter array.  */
	  macro->params = (cpp_hashnode **) BUFF_FRONT (pfile->a_buff);
	  BUFF_FRONT (pfile->a_buff) = (uchar *) &macro->params[macro->paramc];
	  macro->fun_like = 1;
	}
    }

  /* Skip leading whitespace in the replacement text.  */
  CUR (pfile->context)
    = skip_whitespace (pfile, CUR (pfile->context),
		       CPP_OPTION (pfile, discard_comments_in_macro_exp));

  pfile->state.prevent_expansion++;
  scan_out_logical_line (pfile, macro);
  pfile->state.prevent_expansion--;

  if (!macro)
    return false;

  /* Skip trailing white space.  */
  cur = pfile->out.base;
  limit = pfile->out.cur;
  while (limit > cur && is_space (limit[-1]))
    limit--;
  pfile->out.cur = limit;
  save_replacement_text (pfile, macro, 0);

  return true;
}

/* Copy SRC of length LEN to DEST, but convert all contiguous
   whitespace to a single space, provided it is not in quotes.  The
   quote currently in effect is pointed to by PQUOTE, and is updated
   by the function.  Returns the number of bytes copied.  */
static size_t
canonicalize_text (dest, src, len, pquote)
     uchar *dest;
     const uchar *src;
     size_t len;
     uchar *pquote;
{
  uchar *orig_dest = dest;
  uchar quote = *pquote;

  while (len)
    {
      if (is_space (*src) && !quote)
	{
	  do
	    src++, len--;
	  while (len && is_space (*src));
	  *dest++ = ' ';
	}
      else
	{
	  if (*src == '\'' || *src == '"')
	    {
	      if (!quote)
		quote = *src;
	      else if (quote == *src)
		quote = 0;
	    }
	  *dest++ = *src++, len--;
	}
    }

  *pquote = quote;
  return dest - orig_dest;
}

/* Returns true if MACRO1 and MACRO2 have expansions different other
   than in the form of their whitespace.  */
bool
_cpp_expansions_different_trad (macro1, macro2)
     const cpp_macro *macro1, *macro2;
{
  uchar *p1 = xmalloc (macro1->count + macro2->count);
  uchar *p2 = p1 + macro1->count;
  uchar quote1 = 0, quote2;
  bool mismatch;
  size_t len1, len2;

  if (macro1->paramc > 0)
    {
      const uchar *exp1 = macro1->exp.text, *exp2 = macro2->exp.text;

      mismatch = true;
      for (;;)
	{
	  struct block *b1 = (struct block *) exp1;
	  struct block *b2 = (struct block *) exp2;

	  if (b1->arg_index != b2->arg_index)
	    break;

	  len1 = canonicalize_text (p1, b1->text, b1->text_len, &quote1);
	  len2 = canonicalize_text (p2, b2->text, b2->text_len, &quote2);
	  if (len1 != len2 || memcmp (p1, p2, len1))
	    break;
	  if (b1->arg_index == 0)
	    {
	      mismatch = false;
	      break;
	    }
	  exp1 += BLOCK_LEN (b1->text_len);
	  exp2 += BLOCK_LEN (b2->text_len);
	}
    }
  else
    {
      len1 = canonicalize_text (p1, macro1->exp.text, macro1->count, &quote1);
      len2 = canonicalize_text (p2, macro2->exp.text, macro2->count, &quote2);
      mismatch = (len1 != len2 || memcmp (p1, p2, len1));
    }

  free (p1);
  return mismatch;
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

  pfile->out.cur = pfile->out.base;
  CUR (context) = buffer->cur;
  RLIMIT (context) = buffer->rlimit;
  check_output_buffer (pfile, RLIMIT (context) - CUR (context));
}
