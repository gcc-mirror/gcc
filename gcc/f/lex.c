/* Implementation of Fortran lexer
   Copyright (C) 1995, 1996, 1997, 1998, 2001 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "proj.h"
#include "top.h"
#include "bad.h"
#include "com.h"
#include "lex.h"
#include "malloc.h"
#include "src.h"
#include "debug.h"
#include "flags.h"
#include "input.h"
#include "toplev.h"
#include "output.h"
#include "ggc.h"

static void ffelex_append_to_token_ (char c);
static int ffelex_backslash_ (int c, ffewhereColumnNumber col);
static void ffelex_bad_1_ (ffebad errnum, ffewhereLineNumber ln0,
			   ffewhereColumnNumber cn0);
static void ffelex_bad_2_ (ffebad errnum, ffewhereLineNumber ln0,
			   ffewhereColumnNumber cn0, ffewhereLineNumber ln1,
			   ffewhereColumnNumber cn1);
static void ffelex_bad_here_ (int num, ffewhereLineNumber ln0,
			      ffewhereColumnNumber cn0);
static void ffelex_finish_statement_ (void);
static int ffelex_get_directive_line_ (char **text, FILE *finput);
static int ffelex_hash_ (FILE *f);
static ffewhereColumnNumber ffelex_image_char_ (int c,
						ffewhereColumnNumber col);
static void ffelex_include_ (void);
static bool ffelex_is_free_char_ctx_contin_ (ffewhereColumnNumber col);
static bool ffelex_is_free_nonc_ctx_contin_ (ffewhereColumnNumber col);
static void ffelex_next_line_ (void);
static void ffelex_prepare_eos_ (void);
static void ffelex_send_token_ (void);
static ffelexHandler ffelex_swallow_tokens_ (ffelexToken t);
static ffelexToken ffelex_token_new_ (void);

/* Pertaining to the geometry of the input file.  */

/* Initial size for card image to be allocated.  */
#define FFELEX_columnINITIAL_SIZE_ 255

/* The card image itself, which grows as source lines get longer.  It
   has room for ffelex_card_size_ + 8 characters, and the length of the
   current image is ffelex_card_length_.  (The + 8 characters are made
   available for easy handling of tabs and such.)  */
static char *ffelex_card_image_;
static ffewhereColumnNumber ffelex_card_size_;
static ffewhereColumnNumber ffelex_card_length_;

/* Max width for free-form lines (ISO F90).  */
#define FFELEX_FREE_MAX_COLUMNS_ 132

/* True if we saw a tab on the current line, as this (currently) means
   the line is therefore treated as though final_nontab_column_ were
   infinite.  */
static bool ffelex_saw_tab_;

/* TRUE if current line is known to be erroneous, so don't bother
   expanding room for it just to display it.  */
static bool ffelex_bad_line_ = FALSE;

/* Last column for vanilla, i.e. non-tabbed, line.  Usually 72 or 132. */
static ffewhereColumnNumber ffelex_final_nontab_column_;

/* Array for quickly deciding what kind of line the current card has,
   based on its first character.  */
static ffelexType ffelex_first_char_[256];

/* Pertaining to file management.  */

/* The wf argument of the most recent active ffelex_file_(fixed,free)
   function.  */
static ffewhereFile ffelex_current_wf_;

/* TRUE if an INCLUDE statement can be processed (ffelex_set_include
   can be called).  */
static bool ffelex_permit_include_;

/* TRUE if an INCLUDE statement is pending (ffelex_set_include has been
   called).  */
static bool ffelex_set_include_;

/* Information on the pending INCLUDE file.  */
static FILE *ffelex_include_file_;
static bool ffelex_include_free_form_;
static ffewhereFile ffelex_include_wherefile_;

/* Current master line count.  */
static ffewhereLineNumber ffelex_linecount_current_;
/* Next master line count.  */
static ffewhereLineNumber ffelex_linecount_next_;

/* ffewhere info on the latest (currently active) line read from the
   active source file.  */
static ffewhereLine ffelex_current_wl_;
static ffewhereColumn ffelex_current_wc_;

/* Pertaining to tokens in general.  */

/* Initial capacity for text in a CHARACTER/HOLLERITH/NAME/NAMES/NUMBER
   token.  */
#define FFELEX_columnTOKEN_SIZE_ 63
#if FFELEX_columnTOKEN_SIZE_ < FFEWHERE_indexMAX
#error "token size too small!"
#endif

/* Current token being lexed.  */
static ffelexToken ffelex_token_;

/* Handler for current token.  */
static ffelexHandler ffelex_handler_;

/* TRUE if fixed-form lexer is to generate NAMES instead of NAME tokens.  */
static bool ffelex_names_;

/* TRUE if both lexers are to generate NAMES instead of NAME tokens.  */
static bool ffelex_names_pure_;

/* TRUE if 0-9 starts a NAME token instead of NUMBER, for parsing hex
   numbers.  */
static bool ffelex_hexnum_;

/* For ffelex_swallow_tokens().  */
static ffelexHandler ffelex_eos_handler_;

/* Number of tokens sent since last EOS or beginning of input file
   (include INCLUDEd files).  */
static unsigned long int ffelex_number_of_tokens_;

/* Number of labels sent (as NUMBER tokens) since last reset of
   ffelex_number_of_tokens_ to 0, should be 0 or 1 in most cases.
   (Fixed-form source only.)  */
static unsigned long int ffelex_label_tokens_;

/* Metering for token management, to catch token-memory leaks.  */
static long int ffelex_total_tokens_ = 0;
static long int ffelex_old_total_tokens_ = 1;
static long int ffelex_token_nextid_ = 0;

/* Pertaining to lexing CHARACTER and HOLLERITH tokens.  */

/* >0 if a Hollerith constant of that length might be in mid-lex, used
   when the next character seen is 'H' or 'h' to enter HOLLERITH lexing
   mode (see ffelex_raw_mode_).  */
static long int ffelex_expecting_hollerith_;

/* -3: Backslash (escape) sequence being lexed in CHARACTER.
   -2: Possible closing apostrophe/quote seen in CHARACTER.
   -1: Lexing CHARACTER.
    0: Not lexing CHARACTER or HOLLERITH.
   >0: Lexing HOLLERITH, value is # chars remaining to expect.  */
static long int ffelex_raw_mode_;

/* When lexing CHARACTER, open quote/apostrophe (either ' or ").  */
static char ffelex_raw_char_;

/* TRUE when backslash processing had to use most recent character
   to finish its state engine, but that character is not part of
   the backslash sequence, so must be reconsidered as a "normal"
   character in CHARACTER/HOLLERITH lexing.  */
static bool ffelex_backslash_reconsider_ = FALSE;

/* Characters preread before lexing happened (might include EOF).  */
static int *ffelex_kludge_chars_ = NULL;

/* Doing the kludge processing, so not initialized yet.  */
static bool ffelex_kludge_flag_ = FALSE;

/* The beginning of a (possible) CHARACTER/HOLLERITH token.  */
static ffewhereLine ffelex_raw_where_line_;
static ffewhereColumn ffelex_raw_where_col_;


/* Call this to append another character to the current token.	If it isn't
   currently big enough for it, it will be enlarged.  The current token
   must be a CHARACTER, HOLLERITH, NAME, NAMES, or NUMBER.  */

static void
ffelex_append_to_token_ (char c)
{
  if (ffelex_token_->text == NULL)
    {
      ffelex_token_->text
	= malloc_new_ksr (malloc_pool_image (), "FFELEX token text",
			  FFELEX_columnTOKEN_SIZE_ + 1);
      ffelex_token_->size = FFELEX_columnTOKEN_SIZE_;
      ffelex_token_->length = 0;
    }
  else if (ffelex_token_->length >= ffelex_token_->size)
    {
      ffelex_token_->text
	= malloc_resize_ksr (malloc_pool_image (),
			     ffelex_token_->text,
			     (ffelex_token_->size << 1) + 1,
			     ffelex_token_->size + 1);
      ffelex_token_->size <<= 1;
      assert (ffelex_token_->length < ffelex_token_->size);
    }
#ifdef MAP_CHARACTER
Sorry, MAP_CHARACTER is not going to work as expected in GNU Fortran,
please contact fortran@gnu.org if you wish to fund work to
port g77 to non-ASCII machines.
#endif
  ffelex_token_->text[ffelex_token_->length++] = c;
}

/* Do backslash (escape) processing for a CHARACTER/HOLLERITH token
   being lexed.  */

static int
ffelex_backslash_ (int c, ffewhereColumnNumber col)
{
  static int state = 0;
  static unsigned int count;
  static int code;
  static unsigned int firstdig = 0;
  static int nonnull;
  static ffewhereLineNumber line;
  static ffewhereColumnNumber column;

  /* See gcc/c-lex.c readescape() for a straightforward version
     of this state engine for handling backslashes in character/
     hollerith constants.  */

#define wide_flag 0
#define warn_traditional 0
#define flag_traditional 0

  switch (state)
    {
    case 0:
      if ((c == '\\')
	  && (ffelex_raw_mode_ != 0)
	  && ffe_is_backslash ())
	{
	  state = 1;
	  column = col + 1;
	  line = ffelex_linecount_current_;
	  return EOF;
	}
      return c;

    case 1:
      state = 0;		/* Assume simple case. */
      switch (c)
	{
	case 'x':
	  if (warn_traditional)
	    {
	      ffebad_start_msg_lex ("The meaning of `\\x' (at %0) varies with -traditional",
				    FFEBAD_severityWARNING);
	      ffelex_bad_here_ (0, line, column);
	      ffebad_finish ();
	    }

	  if (flag_traditional)
	    return c;

	  code = 0;
	  count = 0;
	  nonnull = 0;
	  state = 2;
	  return EOF;

	case '0':  case '1':  case '2':  case '3':  case '4':
	case '5':  case '6':  case '7':
	  code = c - '0';
	  count = 1;
	  state = 3;
	  return EOF;

	case '\\': case '\'': case '"':
	  return c;

#if 0	/* Inappropriate for Fortran. */
	case '\n':
	  ffelex_next_line_ ();
	  *ignore_ptr = 1;
	  return 0;
#endif

	case 'n':
	  return TARGET_NEWLINE;

	case 't':
	  return TARGET_TAB;

	case 'r':
	  return TARGET_CR;

	case 'f':
	  return TARGET_FF;

	case 'b':
	  return TARGET_BS;

	case 'a':
	  if (warn_traditional)
	    {
	      ffebad_start_msg_lex ("The meaning of `\\a' (at %0) varies with -traditional",
				    FFEBAD_severityWARNING);
	      ffelex_bad_here_ (0, line, column);
	      ffebad_finish ();
	    }

	  if (flag_traditional)
	    return c;
	  return TARGET_BELL;

	case 'v':
#if 0 /* Vertical tab is present in common usage compilers.  */
	  if (flag_traditional)
	    return c;
#endif
	  return TARGET_VT;

	case 'e':
	case 'E':
	case '(':
	case '{':
	case '[':
	case '%':
	  if (pedantic)
	    {
	      char m[2];

	      m[0] = c;
	      m[1] = '\0';
	      ffebad_start_msg_lex ("Non-ISO-C-standard escape sequence `\\%A' at %0",
				    FFEBAD_severityPEDANTIC);
	      ffelex_bad_here_ (0, line, column);
	      ffebad_string (m);
	      ffebad_finish ();
	    }
	  return (c == 'E' || c == 'e') ? 033 : c;

	case '?':
	  return c;

	default:
	  if (c >= 040 && c < 0177)
	    {
	      char m[2];

	      m[0] = c;
	      m[1] = '\0';
	      ffebad_start_msg_lex ("Unknown escape sequence `\\%A' at %0",
				    FFEBAD_severityPEDANTIC);
	      ffelex_bad_here_ (0, line, column);
	      ffebad_string (m);
	      ffebad_finish ();
	    }
	  else if (c == EOF)
	    {
	      ffebad_start_msg_lex ("Unterminated escape sequence `\\' at %0",
				    FFEBAD_severityPEDANTIC);
	      ffelex_bad_here_ (0, line, column);
	      ffebad_finish ();
	    }
	  else
	    {
	      char m[20];

	      sprintf (&m[0], "%x", c);
	      ffebad_start_msg_lex ("Unknown escape sequence `\\' followed by char code 0x%A at %0",
				    FFEBAD_severityPEDANTIC);
	      ffelex_bad_here_ (0, line, column);
	      ffebad_string (m);
	      ffebad_finish ();
	    }
	}
      return c;

    case 2:
      if (ISXDIGIT (c))
	{
	  code = (code * 16) + hex_value (c);
	  if (code != 0 || count != 0)
	    {
	      if (count == 0)
		firstdig = code;
	      count++;
	    }
	  nonnull = 1;
	  return EOF;
	}

      state = 0;

      if (! nonnull)
	{
	  ffebad_start_msg_lex ("\\x used at %0 with no following hex digits",
				FFEBAD_severityFATAL);
	  ffelex_bad_here_ (0, line, column);
	  ffebad_finish ();
	}
      else if (count == 0)
	/* Digits are all 0's.  Ok.  */
	;
      else if ((count - 1) * 4 >= TYPE_PRECISION (integer_type_node)
	       || (count > 1
		   && ((1 << (TYPE_PRECISION (integer_type_node) - (count - 1) * 4))
		       <= (int) firstdig)))
	{
	  ffebad_start_msg_lex ("Hex escape at %0 out of range",
				FFEBAD_severityPEDANTIC);
	  ffelex_bad_here_ (0, line, column);
	  ffebad_finish ();
	}
      break;

    case 3:
      if ((c <= '7') && (c >= '0') && (count++ < 3))
	{
	  code = (code * 8) + (c - '0');
	  return EOF;
	}
      state = 0;
      break;

    default:
      assert ("bad backslash state" == NULL);
      abort ();
    }

  /* Come here when code has a built character, and c is the next
     character that might (or might not) be the next one in the constant.  */

  /* Don't bother doing this check for each character going into
     CHARACTER or HOLLERITH constants, just the escaped-value ones.
     gcc apparently checks every single character, which seems
     like it'd be kinda slow and not worth doing anyway.  */

  if (!wide_flag
      && TYPE_PRECISION (char_type_node) < HOST_BITS_PER_INT
      && code >= (1 << TYPE_PRECISION (char_type_node)))
    {
      ffebad_start_msg_lex ("Escape sequence at %0 out of range for character",
			    FFEBAD_severityFATAL);
      ffelex_bad_here_ (0, line, column);
      ffebad_finish ();
    }

  if (c == EOF)
    {
      /* Known end of constant, just append this character.  */
      ffelex_append_to_token_ (code);
      if (ffelex_raw_mode_ > 0)
	--ffelex_raw_mode_;
      return EOF;
    }

  /* Have two characters to handle.  Do the first, then leave it to the
     caller to detect anything special about the second.  */

  ffelex_append_to_token_ (code);
  if (ffelex_raw_mode_ > 0)
    --ffelex_raw_mode_;
  ffelex_backslash_reconsider_ = TRUE;
  return c;
}

/* ffelex_bad_1_ -- Issue diagnostic with one source point

   ffelex_bad_1_(FFEBAD_SOME_ERROR,ffelex_linecount_current_,column + 1);

   Creates ffewhere line and column objects for the source point, sends them
   along with the error code to ffebad, then kills the line and column
   objects before returning.  */

static void
ffelex_bad_1_ (ffebad errnum, ffewhereLineNumber ln0, ffewhereColumnNumber cn0)
{
  ffewhereLine wl0;
  ffewhereColumn wc0;

  wl0 = ffewhere_line_new (ln0);
  wc0 = ffewhere_column_new (cn0);
  ffebad_start_lex (errnum);
  ffebad_here (0, wl0, wc0);
  ffebad_finish ();
  ffewhere_line_kill (wl0);
  ffewhere_column_kill (wc0);
}

/* ffelex_bad_2_ -- Issue diagnostic with two source points

   ffelex_bad_2_(FFEBAD_SOME_ERROR,ffelex_linecount_current_,column + 1,
	 otherline,othercolumn);

   Creates ffewhere line and column objects for the source points, sends them
   along with the error code to ffebad, then kills the line and column
   objects before returning.  */

static void
ffelex_bad_2_ (ffebad errnum, ffewhereLineNumber ln0, ffewhereColumnNumber cn0,
	       ffewhereLineNumber ln1, ffewhereColumnNumber cn1)
{
  ffewhereLine wl0, wl1;
  ffewhereColumn wc0, wc1;

  wl0 = ffewhere_line_new (ln0);
  wc0 = ffewhere_column_new (cn0);
  wl1 = ffewhere_line_new (ln1);
  wc1 = ffewhere_column_new (cn1);
  ffebad_start_lex (errnum);
  ffebad_here (0, wl0, wc0);
  ffebad_here (1, wl1, wc1);
  ffebad_finish ();
  ffewhere_line_kill (wl0);
  ffewhere_column_kill (wc0);
  ffewhere_line_kill (wl1);
  ffewhere_column_kill (wc1);
}

static void
ffelex_bad_here_ (int n, ffewhereLineNumber ln0,
		  ffewhereColumnNumber cn0)
{
  ffewhereLine wl0;
  ffewhereColumn wc0;

  wl0 = ffewhere_line_new (ln0);
  wc0 = ffewhere_column_new (cn0);
  ffebad_here (n, wl0, wc0);
  ffewhere_line_kill (wl0);
  ffewhere_column_kill (wc0);
}

static int
ffelex_getc_ (FILE *finput)
{
  int c;

  if (ffelex_kludge_chars_ == NULL)
    return getc (finput);

  c = *ffelex_kludge_chars_++;
  if (c != 0)
    return c;

  ffelex_kludge_chars_ = NULL;
  return getc (finput);
}

static int
ffelex_cfebackslash_ (int *use_d, int *d, FILE *finput)
{
  register int c = getc (finput);
  register int code;
  register unsigned count;
  unsigned firstdig = 0;
  int nonnull;

  *use_d = 0;

  switch (c)
    {
    case 'x':
      if (warn_traditional)
	warning ("the meaning of `\\x' varies with -traditional");

      if (flag_traditional)
	return c;

      code = 0;
      count = 0;
      nonnull = 0;
      while (1)
	{
	  c = getc (finput);
	  if (! ISXDIGIT (c))
	    {
	      *use_d = 1;
	      *d = c;
	      break;
	    }
	  code = (code * 16) + hex_value (c);
	  if (code != 0 || count != 0)
	    {
	      if (count == 0)
		firstdig = code;
	      count++;
	    }
	  nonnull = 1;
	}
      if (! nonnull)
	error ("\\x used with no following hex digits");
      else if (count == 0)
	/* Digits are all 0's.  Ok.  */
	;
      else if ((count - 1) * 4 >= TYPE_PRECISION (integer_type_node)
	       || (count > 1
		   && (((unsigned) 1
			<< (TYPE_PRECISION (integer_type_node) - (count - 1)
			    * 4))
		       <= firstdig)))
	pedwarn ("hex escape out of range");
      return code;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':
      code = 0;
      count = 0;
      while ((c <= '7') && (c >= '0') && (count++ < 3))
	{
	  code = (code * 8) + (c - '0');
	  c = getc (finput);
	}
      *use_d = 1;
      *d = c;
      return code;

    case '\\': case '\'': case '"':
      return c;

    case '\n':
      ffelex_next_line_ ();
      *use_d = 2;
      return 0;

    case EOF:
      *use_d = 1;
      *d = EOF;
      return EOF;

    case 'n':
      return TARGET_NEWLINE;

    case 't':
      return TARGET_TAB;

    case 'r':
      return TARGET_CR;

    case 'f':
      return TARGET_FF;

    case 'b':
      return TARGET_BS;

    case 'a':
      if (warn_traditional)
	warning ("the meaning of `\\a' varies with -traditional");

      if (flag_traditional)
	return c;
      return TARGET_BELL;

    case 'v':
#if 0 /* Vertical tab is present in common usage compilers.  */
      if (flag_traditional)
	return c;
#endif
      return TARGET_VT;

    case 'e':
    case 'E':
      if (pedantic)
	pedwarn ("non-ANSI-standard escape sequence, `\\%c'", c);
      return 033;

    case '?':
      return c;

      /* `\(', etc, are used at beginning of line to avoid confusing Emacs.  */
    case '(':
    case '{':
    case '[':
      /* `\%' is used to prevent SCCS from getting confused.  */
    case '%':
      if (pedantic)
	pedwarn ("non-ISO escape sequence `\\%c'", c);
      return c;
    }
  if (c >= 040 && c < 0177)
    pedwarn ("unknown escape sequence `\\%c'", c);
  else
    pedwarn ("unknown escape sequence: `\\' followed by char code 0x%x", c);
  return c;
}

/* A miniature version of the C front-end lexer.  */

static int
ffelex_cfelex_ (ffelexToken *xtoken, FILE *finput, int c)
{
  ffelexToken token;
  char buff[129];
  char *p;
  char *q;
  char *r;
  register unsigned buffer_length;

  if ((*xtoken != NULL) && !ffelex_kludge_flag_)
    ffelex_token_kill (*xtoken);

  switch (c)
    {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      buffer_length = ARRAY_SIZE (buff);
      p = &buff[0];
      q = p;
      r = &buff[buffer_length];
      for (;;)
	{
	  *p++ = c;
	  if (p >= r)
	    {
	      register unsigned bytes_used = (p - q);

	      buffer_length *= 2;
	      q = (char *)xrealloc (q, buffer_length);
	      p = &q[bytes_used];
	      r = &q[buffer_length];
	    }
	  c = ffelex_getc_ (finput);
	  if (! ISDIGIT (c))
	    break;
	}
      *p = '\0';
      token = ffelex_token_new_number (q, ffewhere_line_unknown (),
				       ffewhere_column_unknown ());

      if (q != &buff[0])
	free (q);

      break;

    case '\"':
      buffer_length = ARRAY_SIZE (buff);
      p = &buff[0];
      q = p;
      r = &buff[buffer_length];
      c = ffelex_getc_ (finput);
      for (;;)
	{
	  bool done = FALSE;
	  int use_d = 0;
	  int d;

	  switch (c)
	    {
	    case '\"':
	      c = getc (finput);
	      done = TRUE;
	      break;

	    case '\\':		/* ~~~~~ */
	      c = ffelex_cfebackslash_ (&use_d, &d, finput);
	      break;

	    case EOF:
	    case '\n':
	      error ("badly formed directive -- no closing quote");
	      done = TRUE;
	      break;

	    default:
	      break;
	    }
	  if (done)
	    break;

	  if (use_d != 2)	/* 0=>c, 1=>cd, 2=>nil. */
	    {
	      *p++ = c;
	      if (p >= r)
		{
		  register unsigned bytes_used = (p - q);

		  buffer_length = bytes_used * 2;
		  q = (char *)xrealloc (q, buffer_length);
		  p = &q[bytes_used];
		  r = &q[buffer_length];
		}
	    }
	  if (use_d == 1)
	    c = d;
	  else
	    c = getc (finput);
	}
      *p = '\0';
      token = ffelex_token_new_character (q, ffewhere_line_unknown (),
					  ffewhere_column_unknown ());

      if (q != &buff[0])
	free (q);

      break;

    default:
      token = NULL;
      break;
    }

  *xtoken = token;
  return c;
}

static void
ffelex_file_pop_ (const char *input_filename)
{
  if (input_file_stack->next)
    {
      struct file_stack *p = input_file_stack;
      input_file_stack = p->next;
      free (p);
      input_file_stack_tick++;
      (*debug_hooks->end_source_file) (input_file_stack->line);
    }
  else
    error ("#-lines for entering and leaving files don't match");

  /* Now that we've pushed or popped the input stack,
     update the name in the top element.  */
  if (input_file_stack)
    input_file_stack->name = input_filename;
}

static void
ffelex_file_push_ (int old_lineno, const char *input_filename)
{
  struct file_stack *p
    = (struct file_stack *) xmalloc (sizeof (struct file_stack));

  input_file_stack->line = old_lineno;
  p->next = input_file_stack;
  p->name = input_filename;
  input_file_stack = p;
  input_file_stack_tick++;

  (*debug_hooks->start_source_file) (0, input_filename);

  /* Now that we've pushed or popped the input stack,
     update the name in the top element.  */
  if (input_file_stack)
    input_file_stack->name = input_filename;
}

/* Prepare to finish a statement-in-progress by sending the current
   token, if any, then setting up EOS as the current token with the
   appropriate current pointer.  The caller can then move the current
   pointer before actually sending EOS, if desired, as it is in
   typical fixed-form cases.  */

static void
ffelex_prepare_eos_ ()
{
  if (ffelex_token_->type != FFELEX_typeNONE)
    {
      ffelex_backslash_ (EOF, 0);

      switch (ffelex_raw_mode_)
	{
	case -2:
	  break;

	case -1:
	  ffebad_start_lex ((ffelex_raw_char_ == '\'') ? FFEBAD_NO_CLOSING_APOSTROPHE
			    : FFEBAD_NO_CLOSING_QUOTE);
	  ffebad_here (0, ffelex_token_->where_line, ffelex_token_->where_col);
	  ffebad_here (1, ffelex_current_wl_, ffelex_current_wc_);
	  ffebad_finish ();
	  break;

	case 0:
	  break;

	default:
	  {
	    char num[20];

	    ffebad_start_lex (FFEBAD_NOT_ENOUGH_HOLLERITH_CHARS);
	    ffebad_here (0, ffelex_token_->where_line, ffelex_token_->where_col);
	    ffebad_here (1, ffelex_current_wl_, ffelex_current_wc_);
	    sprintf (num, "%lu", (unsigned long) ffelex_raw_mode_);
	    ffebad_string (num);
	    ffebad_finish ();
	    /* Make sure the token has some text, might as well fill up with spaces.  */
	    do
	      {
		ffelex_append_to_token_ (' ');
	      } while (--ffelex_raw_mode_ > 0);
	    break;
	  }
	}
      ffelex_raw_mode_ = 0;
      ffelex_send_token_ ();
    }
  ffelex_token_->type = FFELEX_typeEOS;
  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
  ffelex_token_->where_col = ffewhere_column_use (ffelex_current_wc_);
}

static void
ffelex_finish_statement_ ()
{
  if ((ffelex_number_of_tokens_ == 0)
      && (ffelex_token_->type == FFELEX_typeNONE))
    return;			/* Don't have a statement pending. */

  if (ffelex_token_->type != FFELEX_typeEOS)
    ffelex_prepare_eos_ ();

  ffelex_permit_include_ = TRUE;
  ffelex_send_token_ ();
  ffelex_permit_include_ = FALSE;
  ffelex_number_of_tokens_ = 0;
  ffelex_label_tokens_ = 0;
  ffelex_names_ = TRUE;
  ffelex_names_pure_ = FALSE;	/* Probably not necessary. */
  ffelex_hexnum_ = FALSE;

  if (!ffe_is_ffedebug ())
    return;

  /* For debugging purposes only. */

  if (ffelex_total_tokens_ != ffelex_old_total_tokens_)
    {
      fprintf (dmpout, "; token_track had %ld tokens, now have %ld.\n",
	       ffelex_old_total_tokens_, ffelex_total_tokens_);
      ffelex_old_total_tokens_ = ffelex_total_tokens_;
    }
}

/* Copied from gcc/c-common.c get_directive_line.  */

static int
ffelex_get_directive_line_ (char **text, FILE *finput)
{
  static char *directive_buffer = NULL;
  static unsigned buffer_length = 0;
  register char *p;
  register char *buffer_limit;
  register int looking_for = 0;
  register int char_escaped = 0;

  if (buffer_length == 0)
    {
      directive_buffer = (char *)xmalloc (128);
      buffer_length = 128;
    }

  buffer_limit = &directive_buffer[buffer_length];

  for (p = directive_buffer; ; )
    {
      int c;

      /* Make buffer bigger if it is full.  */
      if (p >= buffer_limit)
	{
	  register unsigned bytes_used = (p - directive_buffer);

	  buffer_length *= 2;
	  directive_buffer
	    = (char *)xrealloc (directive_buffer, buffer_length);
	  p = &directive_buffer[bytes_used];
	  buffer_limit = &directive_buffer[buffer_length];
	}

      c = getc (finput);

      /* Discard initial whitespace.  */
      if ((c == ' ' || c == '\t') && p == directive_buffer)
	continue;

      /* Detect the end of the directive.  */
      if ((c == '\n' && looking_for == 0)
	  || c == EOF)
	{
	  if (looking_for != 0)
	    error ("bad directive -- missing close-quote");

	  *p++ = '\0';
	  *text = directive_buffer;
	  return c;
	}

      *p++ = c;
      if (c == '\n')
	ffelex_next_line_ ();

      /* Handle string and character constant syntax.  */
      if (looking_for)
	{
	  if (looking_for == c && !char_escaped)
	    looking_for = 0;	/* Found terminator... stop looking.  */
	}
      else
	if (c == '\'' || c == '"')
	  looking_for = c;	/* Don't stop buffering until we see another
				   one of these (or an EOF).  */

      /* Handle backslash.  */
      char_escaped = (c == '\\' && ! char_escaped);
    }
}

/* Handle # directives that make it through (or are generated by) the
   preprocessor.  As much as reasonably possible, emulate the behavior
   of the gcc compiler phase cc1, though interactions between #include
   and INCLUDE might possibly produce bizarre results in terms of
   error reporting and the generation of debugging info vis-a-vis the
   locations of some things.

   Returns the next character unhandled, which is always newline or EOF.  */

#if defined HANDLE_PRAGMA
/* Local versions of these macros, that can be passed as function pointers.  */
static int
pragma_getc ()
{
  return getc (finput);
}

static void
pragma_ungetc (arg)
     int arg;
{
  ungetc (arg, finput);
}
#endif /* HANDLE_PRAGMA */

static int
ffelex_hash_ (FILE *finput)
{
  register int c;
  ffelexToken token = NULL;

  /* Read first nonwhite char after the `#'.  */

  c = ffelex_getc_ (finput);
  while (c == ' ' || c == '\t')
    c = ffelex_getc_ (finput);

  /* If a letter follows, then if the word here is `line', skip
     it and ignore it; otherwise, ignore the line, with an error
     if the word isn't `pragma', `ident', `define', or `undef'.  */

  if (ISALPHA(c))
    {
      if (c == 'p')
	{
	  if (getc (finput) == 'r'
	      && getc (finput) == 'a'
	      && getc (finput) == 'g'
	      && getc (finput) == 'm'
	      && getc (finput) == 'a'
	      && ((c = getc (finput)) == ' ' || c == '\t' || c == '\n'
		  || c == EOF))
	    {
#if 0	/* g77 doesn't handle pragmas, so ignores them FOR NOW. */
	      static char buffer [128];
	      char * buff = buffer;

	      /* Read the pragma name into a buffer.
		 ISSPACE() may evaluate its argument more than once!  */
	      while (((c = getc (finput)), ISSPACE(c)))
		continue;

	      do
		{
		  * buff ++ = c;
		  c = getc (finput);
		}
	      while (c != EOF && ! ISSPACE (c) && c != '\n'
		     && buff < buffer + 128);

	      pragma_ungetc (c);

	      * -- buff = 0;
#ifdef HANDLE_PRAGMA
	      if (HANDLE_PRAGMA (pragma_getc, pragma_ungetc, buffer))
		goto skipline;
#endif /* HANDLE_PRAGMA */
#ifdef HANDLE_GENERIC_PRAGMAS
	      if (handle_generic_pragma (buffer))
		goto skipline;
#endif /* !HANDLE_GENERIC_PRAGMAS */

	      /* Issue a warning message if we have been asked to do so.
		 Ignoring unknown pragmas in system header file unless
		 an explcit -Wunknown-pragmas has been given. */
	      if (warn_unknown_pragmas > 1
		  || (warn_unknown_pragmas && ! in_system_header))
		warning ("ignoring pragma: %s", token_buffer);
#endif /* 0 */
	      goto skipline;
	    }
	}

      else if (c == 'd')
	{
	  if (getc (finput) == 'e'
	      && getc (finput) == 'f'
	      && getc (finput) == 'i'
	      && getc (finput) == 'n'
	      && getc (finput) == 'e'
	      && ((c = getc (finput)) == ' ' || c == '\t' || c == '\n'
		  || c == EOF))
	    {
	      char *text;

	      c = ffelex_get_directive_line_ (&text, finput);

	      if (debug_info_level == DINFO_LEVEL_VERBOSE)
		(*debug_hooks->define) (lineno, text);

	      goto skipline;
	    }
	}
      else if (c == 'u')
	{
	  if (getc (finput) == 'n'
	      && getc (finput) == 'd'
	      && getc (finput) == 'e'
	      && getc (finput) == 'f'
	      && ((c = getc (finput)) == ' ' || c == '\t' || c == '\n'
		  || c == EOF))
	    {
	      char *text;

	      c = ffelex_get_directive_line_ (&text, finput);

	      if (debug_info_level == DINFO_LEVEL_VERBOSE)
		(*debug_hooks->undef) (lineno, text);

	      goto skipline;
	    }
	}
      else if (c == 'l')
	{
	  if (getc (finput) == 'i'
	      && getc (finput) == 'n'
	      && getc (finput) == 'e'
	      && ((c = getc (finput)) == ' ' || c == '\t'))
	    goto linenum;
	}
      else if (c == 'i')
	{
	  if (getc (finput) == 'd'
	      && getc (finput) == 'e'
	      && getc (finput) == 'n'
	      && getc (finput) == 't'
	      && ((c = getc (finput)) == ' ' || c == '\t'))
	    {
	      /* #ident.  The pedantic warning is now in cpp.  */

	      /* Here we have just seen `#ident '.
		 A string constant should follow.  */

	      while (c == ' ' || c == '\t')
		c = getc (finput);

	      /* If no argument, ignore the line.  */
	      if (c == '\n' || c == EOF)
		return c;

	      c = ffelex_cfelex_ (&token, finput, c);

	      if ((token == NULL)
		  || (ffelex_token_type (token) != FFELEX_typeCHARACTER))
		{
		  error ("invalid #ident");
		  goto skipline;
		}

	      if (! flag_no_ident)
		{
#ifdef ASM_OUTPUT_IDENT
		  ASM_OUTPUT_IDENT (asm_out_file,
				    ffelex_token_text (token));
#endif
		}

	      /* Skip the rest of this line.  */
	      goto skipline;
	    }
	}

      error ("undefined or invalid # directive");
      goto skipline;
    }

 linenum:
  /* Here we have either `#line' or `# <nonletter>'.
     In either case, it should be a line number; a digit should follow.  */

  while (c == ' ' || c == '\t')
    c = ffelex_getc_ (finput);

  /* If the # is the only nonwhite char on the line,
     just ignore it.  Check the new newline.  */
  if (c == '\n' || c == EOF)
    return c;

  /* Something follows the #; read a token.  */

  c = ffelex_cfelex_ (&token, finput, c);

  if ((token != NULL)
      && (ffelex_token_type (token) == FFELEX_typeNUMBER))
    {
      int old_lineno = lineno;
      const char *old_input_filename = input_filename;
      ffewhereFile wf;

      /* subtract one, because it is the following line that
	 gets the specified number */
      int l = atoi (ffelex_token_text (token)) - 1;

      /* Is this the last nonwhite stuff on the line?  */
      while (c == ' ' || c == '\t')
	c = ffelex_getc_ (finput);
      if (c == '\n' || c == EOF)
	{
	  /* No more: store the line number and check following line.  */
	  lineno = l;
	  if (!ffelex_kludge_flag_)
	    {
	      ffewhere_file_set (NULL, TRUE, (ffewhereLineNumber) l);

	      if (token != NULL)
		ffelex_token_kill (token);
	    }
	  return c;
	}

      /* More follows: it must be a string constant (filename).  */

      /* Read the string constant.  */
      c = ffelex_cfelex_ (&token, finput, c);

      if ((token == NULL)
	  || (ffelex_token_type (token) != FFELEX_typeCHARACTER))
	{
	  error ("invalid #line");
	  goto skipline;
	}

      lineno = l;

      if (ffelex_kludge_flag_)
	input_filename = ggc_strdup (ffelex_token_text (token));
      else
	{
	  wf = ffewhere_file_new (ffelex_token_text (token),
				  ffelex_token_length (token));
	  input_filename = ffewhere_file_name (wf);
	  ffewhere_file_set (wf, TRUE, (ffewhereLineNumber) l);
	}

#if 0	/* Not sure what g77 should do with this yet. */
      /* Each change of file name
	 reinitializes whether we are now in a system header.  */
      in_system_header = 0;
#endif

      if (main_input_filename == 0)
	main_input_filename = input_filename;

      /* Is this the last nonwhite stuff on the line?  */
      while (c == ' ' || c == '\t')
	c = getc (finput);
      if (c == '\n' || c == EOF)
	{
	  if (!ffelex_kludge_flag_)
	    {
	      /* Update the name in the top element of input_file_stack.  */
	      if (input_file_stack)
		input_file_stack->name = input_filename;

	      if (token != NULL)
		ffelex_token_kill (token);
	    }
	  return c;
	}

      c = ffelex_cfelex_ (&token, finput, c);

      /* `1' after file name means entering new file.
	 `2' after file name means just left a file.  */

      if ((token != NULL)
	  && (ffelex_token_type (token) == FFELEX_typeNUMBER))
	{
	  int num = atoi (ffelex_token_text (token));

	  if (ffelex_kludge_flag_)
	    {
	      lineno = 1;
	      input_filename = old_input_filename;
	      error ("use `#line ...' instead of `# ...' in first line");
	    }

	  if (num == 1)
	    {
	      /* Pushing to a new file.  */
	      ffelex_file_push_ (old_lineno, input_filename);
	    }
	  else if (num == 2)
	    {
	      /* Popping out of a file.  */
	      ffelex_file_pop_ (input_filename);
	    }

	  /* Is this the last nonwhite stuff on the line?  */
	  while (c == ' ' || c == '\t')
	    c = getc (finput);
	  if (c == '\n' || c == EOF)
	    {
	      if (token != NULL)
		ffelex_token_kill (token);
	      return c;
	    }

	  c = ffelex_cfelex_ (&token, finput, c);
	}

      /* `3' after file name means this is a system header file.  */

#if 0	/* Not sure what g77 should do with this yet. */
      if ((token != NULL)
	  && (ffelex_token_type (token) == FFELEX_typeNUMBER)
	  && (atoi (ffelex_token_text (token)) == 3))
	in_system_header = 1;
#endif

      while (c == ' ' || c == '\t')
	c = getc (finput);
      if (((token != NULL)
	   || (c != '\n' && c != EOF))
	  && ffelex_kludge_flag_)
	{
	  lineno = 1;
	  input_filename = old_input_filename;
	  error ("use `#line ...' instead of `# ...' in first line");
	}
      if (c == '\n' || c == EOF)
	{
	  if (token != NULL && !ffelex_kludge_flag_)
	    ffelex_token_kill (token);
	  return c;
	}
    }
  else
    error ("invalid #-line");

  /* skip the rest of this line.  */
 skipline:
  if ((token != NULL) && !ffelex_kludge_flag_)
    ffelex_token_kill (token);
  while ((c = getc (finput)) != EOF && c != '\n')
    ;
  return c;
}

/* "Image" a character onto the card image, return incremented column number.

   Normally invoking this function as in
     column = ffelex_image_char_ (c, column);
   is the same as doing:
     ffelex_card_image_[column++] = c;

   However, tabs and carriage returns are handled specially, to preserve
   the visual "image" of the input line (in most editors) in the card
   image.

   Carriage returns are ignored, as they are assumed to be followed
   by newlines.

   A tab is handled by first doing:
     ffelex_card_image_[column++] = ' ';
   That is, it translates to at least one space.  Then, as many spaces
   are imaged as necessary to bring the column number to the next tab
   position, where tab positions start in the ninth column and each
   eighth column afterwards.  ALSO, a static var named ffelex_saw_tab_
   is set to TRUE to notify the lexer that a tab was seen.

   Columns are numbered and tab stops set as illustrated below:

   012345670123456701234567...
   x	   y	   z
   xx	   yy	   zz
   ...
   xxxxxxx yyyyyyy zzzzzzz
   xxxxxxxx	   yyyyyyyy...  */

static ffewhereColumnNumber
ffelex_image_char_ (int c, ffewhereColumnNumber column)
{
  ffewhereColumnNumber old_column = column;

  if (column >= ffelex_card_size_)
    {
      ffewhereColumnNumber newmax = ffelex_card_size_ << 1;

      if (ffelex_bad_line_)
	return column;

      if ((newmax >> 1) != ffelex_card_size_)
	{			/* Overflowed column number. */
	overflow:	/* :::::::::::::::::::: */

	  ffelex_bad_line_ = TRUE;
	  strcpy (&ffelex_card_image_[column - 3], "...");
	  ffelex_card_length_ = column;
	  ffelex_bad_1_ (FFEBAD_LINE_TOO_LONG,
			 ffelex_linecount_current_, column + 1);
	  return column;
	}

      ffelex_card_image_
	= malloc_resize_ksr (malloc_pool_image (),
			     ffelex_card_image_,
			     newmax + 9,
			     ffelex_card_size_ + 9);
      ffelex_card_size_ = newmax;
    }

  switch (c)
    {
    case '\r':
      break;

    case '\t':
      ffelex_saw_tab_ = TRUE;
      ffelex_card_image_[column++] = ' ';
      while ((column & 7) != 0)
	ffelex_card_image_[column++] = ' ';
      break;

    case '\0':
      if (!ffelex_bad_line_)
	{
	  ffelex_bad_line_ = TRUE;
	  strcpy (&ffelex_card_image_[column], "[\\0]");
	  ffelex_card_length_ = column + 4;
	  ffebad_start_msg_lex ("Null character at %0 -- line ignored",
				FFEBAD_severityFATAL);
	  ffelex_bad_here_ (0, ffelex_linecount_current_, column + 1);
	  ffebad_finish ();
	  column += 4;
	}
      break;

    default:
      ffelex_card_image_[column++] = c;
      break;
    }

  if (column < old_column)
    {
      column = old_column;
      goto overflow;	/* :::::::::::::::::::: */
    }

  return column;
}

static void
ffelex_include_ ()
{
  ffewhereFile include_wherefile = ffelex_include_wherefile_;
  FILE *include_file = ffelex_include_file_;
  /* The rest of this is to push, and after the INCLUDE file is processed,
     pop, the static lexer state info that pertains to each particular
     input file.  */
  char *card_image;
  ffewhereColumnNumber card_size = ffelex_card_size_;
  ffewhereColumnNumber card_length = ffelex_card_length_;
  ffewhereLine current_wl = ffelex_current_wl_;
  ffewhereColumn current_wc = ffelex_current_wc_;
  bool saw_tab = ffelex_saw_tab_;
  ffewhereColumnNumber final_nontab_column = ffelex_final_nontab_column_;
  ffewhereFile current_wf = ffelex_current_wf_;
  ffewhereLineNumber linecount_current = ffelex_linecount_current_;
  ffewhereLineNumber linecount_offset
    = ffewhere_line_filelinenum (current_wl);
  int old_lineno = lineno;
  const char *old_input_filename = input_filename;

  if (card_length != 0)
    {
      card_image = malloc_new_ks (malloc_pool_image (),
				  "FFELEX saved card image",
				  card_length);
      memcpy (card_image, ffelex_card_image_, card_length);
    }
  else
    card_image = NULL;

  ffelex_set_include_ = FALSE;

  ffelex_next_line_ ();

  ffewhere_file_set (include_wherefile, TRUE, 0);

  ffelex_file_push_ (old_lineno, ffewhere_file_name (include_wherefile));

  if (ffelex_include_free_form_)
    ffelex_file_free (include_wherefile, include_file);
  else
    ffelex_file_fixed (include_wherefile, include_file);

  ffelex_file_pop_ (ffewhere_file_name (current_wf));

  ffewhere_file_set (current_wf, TRUE, linecount_offset);

  ffecom_close_include (include_file);

  if (card_length != 0)
    {
#ifdef REDUCE_CARD_SIZE_AFTER_BIGGY	/* Define if occasional large lines. */
#error "need to handle possible reduction of card size here!!"
#endif
      assert (ffelex_card_size_ >= card_length);	/* It shrunk?? */
      memcpy (ffelex_card_image_, card_image, card_length);
    }
  ffelex_card_image_[card_length] = '\0';

  input_filename = old_input_filename;
  lineno = old_lineno;
  ffelex_linecount_current_ = linecount_current;
  ffelex_current_wf_ = current_wf;
  ffelex_final_nontab_column_ = final_nontab_column;
  ffelex_saw_tab_ = saw_tab;
  ffelex_current_wc_ = current_wc;
  ffelex_current_wl_ = current_wl;
  ffelex_card_length_ = card_length;
  ffelex_card_size_ = card_size;
}

/* ffelex_is_free_char_ctx_contin_ -- Character Context Continuation?

   ffewhereColumnNumber col;
   int c;  // Char at col.
   if ((c == '&') && ffelex_is_free_char_ctx_contin_(col + 1))
       // We have a continuation indicator.

   If there are <n> spaces starting at ffelex_card_image_[col] up through
   the null character, where <n> is 0 or greater, returns TRUE.	 */

static bool
ffelex_is_free_char_ctx_contin_ (ffewhereColumnNumber col)
{
  while (ffelex_card_image_[col] != '\0')
    {
      if (ffelex_card_image_[col++] != ' ')
	return FALSE;
    }
  return TRUE;
}

/* ffelex_is_free_nonc_ctx_contin_ -- Noncharacter Context Continuation?

   ffewhereColumnNumber col;
   int c;  // Char at col.
   if ((c == '&') && ffelex_is_free_nonc_ctx_contin_(col + 1))
       // We have a continuation indicator.

   If there are <n> spaces starting at ffelex_card_image_[col] up through
   the null character or '!', where <n> is 0 or greater, returns TRUE.	*/

static bool
ffelex_is_free_nonc_ctx_contin_ (ffewhereColumnNumber col)
{
  while ((ffelex_card_image_[col] != '\0') && (ffelex_card_image_[col] != '!'))
    {
      if (ffelex_card_image_[col++] != ' ')
	return FALSE;
    }
  return TRUE;
}

static void
ffelex_next_line_ ()
{
  ffelex_linecount_current_ = ffelex_linecount_next_;
  ++ffelex_linecount_next_;
  ++lineno;
}

static void
ffelex_send_token_ ()
{
  ++ffelex_number_of_tokens_;

  ffelex_backslash_ (EOF, 0);

  if (ffelex_token_->text == NULL)
    {
      if (ffelex_token_->type == FFELEX_typeCHARACTER)
	{
	  ffelex_append_to_token_ ('\0');
	  ffelex_token_->length = 0;
	}
    }
  else
    ffelex_token_->text[ffelex_token_->length] = '\0';

  assert (ffelex_raw_mode_ == 0);

  if (ffelex_token_->type == FFELEX_typeNAMES)
    {
      ffewhere_line_kill (ffelex_token_->currentnames_line);
      ffewhere_column_kill (ffelex_token_->currentnames_col);
    }

  assert (ffelex_handler_ != NULL);
  ffelex_handler_ = (ffelexHandler) (*ffelex_handler_) (ffelex_token_);
  assert (ffelex_handler_ != NULL);

  ffelex_token_kill (ffelex_token_);

  ffelex_token_ = ffelex_token_new_ ();
  ffelex_token_->uses = 1;
  ffelex_token_->text = NULL;
  if (ffelex_raw_mode_ < 0)
    {
      ffelex_token_->type = FFELEX_typeCHARACTER;
      ffelex_token_->where_line = ffelex_raw_where_line_;
      ffelex_token_->where_col = ffelex_raw_where_col_;
      ffelex_raw_where_line_ = ffewhere_line_unknown ();
      ffelex_raw_where_col_ = ffewhere_column_unknown ();
    }
  else
    {
      ffelex_token_->type = FFELEX_typeNONE;
      ffelex_token_->where_line = ffewhere_line_unknown ();
      ffelex_token_->where_col = ffewhere_column_unknown ();
    }

  if (ffelex_set_include_)
    ffelex_include_ ();
}

/* ffelex_swallow_tokens_ -- Eat all tokens delivered to me

   return ffelex_swallow_tokens_;

   Return this handler when you don't want to look at any more tokens in the
   statement because you've encountered an unrecoverable error in the
   statement.  */

static ffelexHandler
ffelex_swallow_tokens_ (ffelexToken t)
{
  assert (ffelex_eos_handler_ != NULL);

  if ((ffelex_token_type (t) == FFELEX_typeEOS)
      || (ffelex_token_type (t) == FFELEX_typeSEMICOLON))
    return (ffelexHandler) (*ffelex_eos_handler_) (t);

  return (ffelexHandler) ffelex_swallow_tokens_;
}

static ffelexToken
ffelex_token_new_ ()
{
  ffelexToken t;

  ++ffelex_total_tokens_;

  t = (ffelexToken) malloc_new_ks (malloc_pool_image (),
				   "FFELEX token", sizeof (*t));
  t->id_ = ffelex_token_nextid_++;
  return t;
}

static const char *
ffelex_type_string_ (ffelexType type)
{
  static const char *const types[] = {
    "FFELEX_typeNONE",
    "FFELEX_typeCOMMENT",
    "FFELEX_typeEOS",
    "FFELEX_typeEOF",
    "FFELEX_typeERROR",
    "FFELEX_typeRAW",
    "FFELEX_typeQUOTE",
    "FFELEX_typeDOLLAR",
    "FFELEX_typeHASH",
    "FFELEX_typePERCENT",
    "FFELEX_typeAMPERSAND",
    "FFELEX_typeAPOSTROPHE",
    "FFELEX_typeOPEN_PAREN",
    "FFELEX_typeCLOSE_PAREN",
    "FFELEX_typeASTERISK",
    "FFELEX_typePLUS",
    "FFELEX_typeMINUS",
    "FFELEX_typePERIOD",
    "FFELEX_typeSLASH",
    "FFELEX_typeNUMBER",
    "FFELEX_typeOPEN_ANGLE",
    "FFELEX_typeEQUALS",
    "FFELEX_typeCLOSE_ANGLE",
    "FFELEX_typeNAME",
    "FFELEX_typeCOMMA",
    "FFELEX_typePOWER",
    "FFELEX_typeCONCAT",
    "FFELEX_typeDEBUG",
    "FFELEX_typeNAMES",
    "FFELEX_typeHOLLERITH",
    "FFELEX_typeCHARACTER",
    "FFELEX_typeCOLON",
    "FFELEX_typeSEMICOLON",
    "FFELEX_typeUNDERSCORE",
    "FFELEX_typeQUESTION",
    "FFELEX_typeOPEN_ARRAY",
    "FFELEX_typeCLOSE_ARRAY",
    "FFELEX_typeCOLONCOLON",
    "FFELEX_typeREL_LE",
    "FFELEX_typeREL_NE",
    "FFELEX_typeREL_EQ",
    "FFELEX_typePOINTS",
    "FFELEX_typeREL_GE"
  };

  if (type >= ARRAY_SIZE (types))
    return "???";
  return types[type];
}

void
ffelex_display_token (ffelexToken t)
{
  if (t == NULL)
    t = ffelex_token_;

  fprintf (dmpout, "; Token #%lu is %s (line %" ffewhereLineNumber_f "u, col %"
	   ffewhereColumnNumber_f "u)",
	   t->id_,
	   ffelex_type_string_ (t->type),
	   ffewhere_line_number (t->where_line),
	   ffewhere_column_number (t->where_col));

  if (t->text != NULL)
    fprintf (dmpout, ": \"%.*s\"\n",
	     (int) t->length,
	     t->text);
  else
    fprintf (dmpout, ".\n");
}

/* ffelex_expecting_character -- Tells if next token expected to be CHARACTER

   if (ffelex_expecting_character())
       // next token delivered by lexer will be CHARACTER.

   If the most recent call to ffelex_set_expecting_hollerith since the last
   token was delivered by the lexer passed a length of -1, then we return
   TRUE, because the next token we deliver will be typeCHARACTER, else we
   return FALSE.  */

bool
ffelex_expecting_character ()
{
  return (ffelex_raw_mode_ != 0);
}

/* ffelex_file_fixed -- Lex a given file in fixed source form

   ffewhere wf;
   FILE *f;
   ffelex_file_fixed(wf,f);

   Lexes the file according to Fortran 90 ANSI + VXT specifications.  */

ffelexHandler
ffelex_file_fixed (ffewhereFile wf, FILE *f)
{
  register int c = 0;		/* Character currently under consideration. */
  register ffewhereColumnNumber column = 0;	/* Not really; 0 means column 1... */
  bool disallow_continuation_line;
  bool ignore_disallowed_continuation = FALSE;
  int latest_char_in_file = 0;	/* For getting back into comment-skipping
				   code. */
  ffelexType lextype;
  ffewhereColumnNumber first_label_char;	/* First char of label --
						   column number. */
  char label_string[6];		/* Text of label. */
  int labi;			/* Length of label text. */
  bool finish_statement;	/* Previous statement finished? */
  bool have_content;		/* This line have content? */
  bool just_do_label;		/* Nothing but label (and continuation?) on
				   line. */

  /* Lex is called for a particular file, not for a particular program unit.
     Yet the two events do share common characteristics.  The first line in a
     file or in a program unit cannot be a continuation line.  No token can
     be in mid-formation.  No current label for the statement exists, since
     there is no current statement. */

  assert (ffelex_handler_ != NULL);

  lineno = 0;
  input_filename = ffewhere_file_name (wf);
  ffelex_current_wf_ = wf;
  disallow_continuation_line = TRUE;
  ignore_disallowed_continuation = FALSE;
  ffelex_token_->type = FFELEX_typeNONE;
  ffelex_number_of_tokens_ = 0;
  ffelex_label_tokens_ = 0;
  ffelex_current_wl_ = ffewhere_line_unknown ();
  ffelex_current_wc_ = ffewhere_column_unknown ();
  latest_char_in_file = '\n';

  goto first_line;		/* :::::::::::::::::::: */

  /* Come here to get a new line. */

 beginning_of_line:		/* :::::::::::::::::::: */

  disallow_continuation_line = FALSE;

  /* Come here directly when last line didn't clarify the continuation issue. */

 beginning_of_line_again:	/* :::::::::::::::::::: */

#ifdef REDUCE_CARD_SIZE_AFTER_BIGGY	/* Define if occasional large lines. */
  if (ffelex_card_size_ != FFELEX_columnINITIAL_SIZE_)
    {
      ffelex_card_image_
	= malloc_resize_ks (malloc_pool_image (),
			    ffelex_card_image_,
			    FFELEX_columnINITIAL_SIZE_ + 9,
			    ffelex_card_size_ + 9);
      ffelex_card_size_ = FFELEX_columnINITIAL_SIZE_;
    }
#endif

 first_line:			/* :::::::::::::::::::: */

  c = latest_char_in_file;
  if ((c == EOF) || ((c = ffelex_getc_ (f)) == EOF))
    {

    end_of_file:		/* :::::::::::::::::::: */

      /* Line ending in EOF instead of \n still counts as a whole line. */

      ffelex_finish_statement_ ();
      ffewhere_line_kill (ffelex_current_wl_);
      ffewhere_column_kill (ffelex_current_wc_);
      return (ffelexHandler) ffelex_handler_;
    }

  ffelex_next_line_ ();

  ffelex_bad_line_ = FALSE;

  /* Skip over comment (and otherwise ignored) lines as quickly as possible! */

  while (((lextype = ffelex_first_char_[c]) == FFELEX_typeCOMMENT)
	 || (lextype == FFELEX_typeERROR)
	 || (lextype == FFELEX_typeSLASH)
	 || (lextype == FFELEX_typeHASH))
    {
      /* Test most frequent type of line first, etc.  */
      if ((lextype == FFELEX_typeCOMMENT)
	  || ((lextype == FFELEX_typeSLASH)
	      && ((c = getc (f)) == '*')))	/* NOTE SIDE-EFFECT. */
	{
	  /* Typical case (straight comment), just ignore rest of line. */
	comment_line:		/* :::::::::::::::::::: */

	  while ((c != '\n') && (c != EOF))
	    c = getc (f);
	}
      else if (lextype == FFELEX_typeHASH)
	c = ffelex_hash_ (f);
      else if (lextype == FFELEX_typeSLASH)
	{
	  /* SIDE-EFFECT ABOVE HAS HAPPENED. */
	  ffelex_card_image_[0] = '/';
	  ffelex_card_image_[1] = c;
	  column = 2;
	  goto bad_first_character;	/* :::::::::::::::::::: */
	}
      else
	/* typeERROR or unsupported typeHASH.  */
	{			/* Bad first character, get line and display
				   it with message. */
	  column = ffelex_image_char_ (c, 0);

	bad_first_character:	/* :::::::::::::::::::: */

	  ffelex_bad_line_ = TRUE;
	  while (((c = getc (f)) != '\n') && (c != EOF))
	    column = ffelex_image_char_ (c, column);
	  ffelex_card_image_[column] = '\0';
	  ffelex_card_length_ = column;
	  ffelex_bad_1_ (FFEBAD_FIRST_CHAR_INVALID,
			 ffelex_linecount_current_, 1);
	}

      /* Read past last char in line.  */

      if (c == EOF)
	{
	  ffelex_next_line_ ();
	  goto end_of_file;	/* :::::::::::::::::::: */
	}

      c = getc (f);

      ffelex_next_line_ ();

      if (c == EOF)
	goto end_of_file;	/* :::::::::::::::::::: */

      ffelex_bad_line_ = FALSE;
    }				/* while [c, first char, means comment] */

  ffelex_saw_tab_
    = (c == '&')
      || (ffelex_final_nontab_column_ == 0);

  if (lextype == FFELEX_typeDEBUG)
    c = ' ';			/* A 'D' or 'd' in column 1 with the
				   debug-lines option on. */

  column = ffelex_image_char_ (c, 0);

  /* Read the entire line in as is (with whitespace processing).  */

  while (((c = getc (f)) != '\n') && (c != EOF))
    column = ffelex_image_char_ (c, column);

  if (ffelex_bad_line_)
    {
      ffelex_card_image_[column] = '\0';
      ffelex_card_length_ = column;
      goto comment_line;		/* :::::::::::::::::::: */
    }

  /* If no tab, cut off line after column 72/132.  */

  if (!ffelex_saw_tab_ && (column > ffelex_final_nontab_column_))
    {
      /* Technically, we should now fill ffelex_card_image_ up thru column
	 72/132 with spaces, since character/hollerith constants must count
	 them in that manner. To save CPU time in several ways (avoid a loop
	 here that would be used only when we actually end a line in
	 character-constant mode; avoid writing memory unnecessarily; avoid a
	 loop later checking spaces when not scanning for character-constant
	 characters), we don't do this, and we do the appropriate thing when
	 we encounter end-of-line while actually processing a character
	 constant. */

      column = ffelex_final_nontab_column_;
    }

  ffelex_card_image_[column] = '\0';
  ffelex_card_length_ = column;

  /* Save next char in file so we can use register-based c while analyzing
     line we just read. */

  latest_char_in_file = c;	/* Should be either '\n' or EOF. */

  have_content = FALSE;

  /* Handle label, if any. */

  labi = 0;
  first_label_char = FFEWHERE_columnUNKNOWN;
  for (column = 0; column < 5; ++column)
    {
      switch (c = ffelex_card_image_[column])
	{
	case '\0':
	case '!':
	  goto stop_looking;	/* :::::::::::::::::::: */

	case ' ':
	  break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  label_string[labi++] = c;
	  if (first_label_char == FFEWHERE_columnUNKNOWN)
	    first_label_char = column + 1;
	  break;

	case '&':
	  if (column != 0)
	    {
	      ffelex_bad_1_ (FFEBAD_LABEL_FIELD_NOT_NUMERIC,
			     ffelex_linecount_current_,
			     column + 1);
	      goto beginning_of_line_again;	/* :::::::::::::::::::: */
	    }
	  if (ffe_is_pedantic ())
	    ffelex_bad_1_ (FFEBAD_AMPERSAND,
			   ffelex_linecount_current_, 1);
	  finish_statement = FALSE;
	  just_do_label = FALSE;
	  goto got_a_continuation;	/* :::::::::::::::::::: */

	case '/':
	  if (ffelex_card_image_[column + 1] == '*')
	    goto stop_looking;	/* :::::::::::::::::::: */
	  /* Fall through. */
	default:
	  ffelex_bad_1_ (FFEBAD_LABEL_FIELD_NOT_NUMERIC,
			 ffelex_linecount_current_, column + 1);
	  goto beginning_of_line_again;	/* :::::::::::::::::::: */
	}
    }

 stop_looking:			/* :::::::::::::::::::: */

  label_string[labi] = '\0';

  /* Find first nonblank char starting with continuation column. */

  if (column == 5)		/* In which case we didn't see end of line in
				   label field. */
    while ((c = ffelex_card_image_[column]) == ' ')
      ++column;

  /* Now we're trying to figure out whether this is a continuation line and
     whether there's anything else of substance on the line.  The cases are
     as follows:

     1. If a line has an explicit continuation character (other than the digit
     zero), then if it also has a label, the label is ignored and an error
     message is printed.  Any remaining text on the line is passed to the
     parser tasks, thus even an all-blank line (possibly with an ignored
     label) aside from a positive continuation character might have meaning
     in the midst of a character or hollerith constant.

     2. If a line has no explicit continuation character (that is, it has a
     space in column 6 and the first non-space character past column 6 is
     not a digit 0-9), then there are two possibilities:

     A. A label is present and/or a non-space (and non-comment) character
     appears somewhere after column 6.	Terminate processing of the previous
     statement, if any, send the new label for the next statement, if any,
     and start processing a new statement with this non-blank character, if
     any.

     B. The line is essentially blank, except for a possible comment character.
     Don't terminate processing of the previous statement and don't pass any
     characters to the parser tasks, since the line is not flagged as a
     continuation line.	 We treat it just like a completely blank line.

     3. If a line has a continuation character of zero (0), then we terminate
     processing of the previous statement, if any, send the new label for the
     next statement, if any, and start processing a new statement, if any
     non-blank characters are present.

     If, when checking to see if we should terminate the previous statement, it
     is found that there is no previous statement but that there is an
     outstanding label, substitute CONTINUE as the statement for the label
     and display an error message. */

  finish_statement = FALSE;
  just_do_label = FALSE;

  switch (c)
    {
    case '!':			/* ANSI Fortran 90 says ! in column 6 is
				   continuation. */
      /* VXT Fortran says ! anywhere is comment, even column 6. */
      if (ffe_is_vxt () || (column != 5))
	goto no_tokens_on_line;	/* :::::::::::::::::::: */
      goto got_a_continuation;	/* :::::::::::::::::::: */

    case '/':
      if (ffelex_card_image_[column + 1] != '*')
	goto some_other_character;	/* :::::::::::::::::::: */
      /* Fall through. */
      if (column == 5)
	{
	  /* This seems right to do. But it is close to call, since / * starting
	     in column 6 will thus be interpreted as a continuation line
	     beginning with '*'. */

	  goto got_a_continuation;/* :::::::::::::::::::: */
	}
      /* Fall through. */
    case '\0':
      /* End of line.  Therefore may be continued-through line, so handle
	 pending label as possible to-be-continued and drive end-of-statement
	 for any previous statement, else treat as blank line. */

     no_tokens_on_line:		/* :::::::::::::::::::: */

      if (ffe_is_pedantic () && (c == '/'))
	ffelex_bad_1_ (FFEBAD_NON_ANSI_COMMENT,
		       ffelex_linecount_current_, column + 1);
      if (first_label_char != FFEWHERE_columnUNKNOWN)
	{			/* Can't be a continued-through line if it
				   has a label. */
	  finish_statement = TRUE;
	  have_content = TRUE;
	  just_do_label = TRUE;
	  break;
	}
      goto beginning_of_line_again;	/* :::::::::::::::::::: */

    case '0':
      if (ffe_is_pedantic () && (column != 5))
	ffelex_bad_1_ (FFEBAD_NON_ANSI_CONTINUATION_COLUMN,
		       ffelex_linecount_current_, column + 1);
      finish_statement = TRUE;
      goto check_for_content;	/* :::::::::::::::::::: */

    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':

      /* NOTE: This label can be reached directly from the code
	 that lexes the label field in columns 1-5.  */
     got_a_continuation:	/* :::::::::::::::::::: */

      if (first_label_char != FFEWHERE_columnUNKNOWN)
	{
	  ffelex_bad_2_ (FFEBAD_LABEL_ON_CONTINUATION,
			 ffelex_linecount_current_,
			 first_label_char,
			 ffelex_linecount_current_,
			 column + 1);
	  first_label_char = FFEWHERE_columnUNKNOWN;
	}
      if (disallow_continuation_line)
	{
	  if (!ignore_disallowed_continuation)
	    ffelex_bad_1_ (FFEBAD_INVALID_CONTINUATION,
			   ffelex_linecount_current_, column + 1);
	  goto beginning_of_line_again;	/* :::::::::::::::::::: */
	}
      if (ffe_is_pedantic () && (column != 5))
	ffelex_bad_1_ (FFEBAD_NON_ANSI_CONTINUATION_COLUMN,
		       ffelex_linecount_current_, column + 1);
      if ((ffelex_raw_mode_ != 0)
	  && (((c = ffelex_card_image_[column + 1]) != '\0')
	      || !ffelex_saw_tab_))
	{
	  ++column;
	  have_content = TRUE;
	  break;
	}

     check_for_content:		/* :::::::::::::::::::: */

      while ((c = ffelex_card_image_[++column]) == ' ')
	;
      if ((c == '\0')
	  || (c == '!')
	  || ((c == '/')
	      && (ffelex_card_image_[column + 1] == '*')))
	{
	  if (ffe_is_pedantic () && (c == '/'))
	    ffelex_bad_1_ (FFEBAD_NON_ANSI_COMMENT,
			   ffelex_linecount_current_, column + 1);
	  just_do_label = TRUE;
	}
      else
	have_content = TRUE;
      break;

    default:

     some_other_character:	/* :::::::::::::::::::: */

      if (column == 5)
	goto got_a_continuation;/* :::::::::::::::::::: */

      /* Here is the very normal case of a regular character starting in
	 column 7 or beyond with a blank in column 6. */

      finish_statement = TRUE;
      have_content = TRUE;
      break;
    }

  if (have_content
      || (first_label_char != FFEWHERE_columnUNKNOWN))
    {
      /* The line has content of some kind, install new end-statement
	 point for error messages.  Note that "content" includes cases
	 where there's little apparent content but enough to finish
	 a statement.  That's because finishing a statement can trigger
	 an impending INCLUDE, and that requires accurate line info being
	 maintained by the lexer.  */

      if (finish_statement)
	ffelex_prepare_eos_ ();	/* Prepare EOS before we move current pointer. */

      ffewhere_line_kill (ffelex_current_wl_);
      ffewhere_column_kill (ffelex_current_wc_);
      ffelex_current_wl_ = ffewhere_line_new (ffelex_linecount_current_);
      ffelex_current_wc_ = ffewhere_column_new (ffelex_card_length_ + 1);
    }

  /* We delay this for a combination of reasons.  Mainly, it can start
     INCLUDE processing, and we want to delay that until the lexer's
     info on the line is coherent.  And we want to delay that until we're
     sure there's a reason to make that info coherent, to avoid saving
     lots of useless lines.  */

  if (finish_statement)
    ffelex_finish_statement_ ();

  /* If label is present, enclose it in a NUMBER token and send it along. */

  if (first_label_char != FFEWHERE_columnUNKNOWN)
    {
      assert (ffelex_token_->type == FFELEX_typeNONE);
      ffelex_token_->type = FFELEX_typeNUMBER;
      ffelex_append_to_token_ ('\0');	/* Make room for label text. */
      strcpy (ffelex_token_->text, label_string);
      ffelex_token_->where_line
	= ffewhere_line_use (ffelex_current_wl_);
      ffelex_token_->where_col = ffewhere_column_new (first_label_char);
      ffelex_token_->length = labi;
      ffelex_send_token_ ();
      ++ffelex_label_tokens_;
    }

  if (just_do_label)
    goto beginning_of_line;	/* :::::::::::::::::::: */

  /* Here is the main engine for parsing.  c holds the character at column.
     It is already known that c is not a blank, end of line, or shriek,
     unless ffelex_raw_mode_ is not 0 (indicating we are in a
     character/hollerith constant). A partially filled token may already
     exist in ffelex_token_.  One special case: if, when the end of the line
     is reached, continuation_line is FALSE and the only token on the line is
     END, then it is indeed the last statement. We don't look for
     continuation lines during this program unit in that case. This is
     according to ANSI. */

  if (ffelex_raw_mode_ != 0)
    {

    parse_raw_character:	/* :::::::::::::::::::: */

      if (c == '\0')
	{
	  ffewhereColumnNumber i;

	  if (ffelex_saw_tab_ || (column >= ffelex_final_nontab_column_))
	    goto beginning_of_line;	/* :::::::::::::::::::: */

	  /* Pad out line with "virtual" spaces. */

	  for (i = column; i < ffelex_final_nontab_column_; ++i)
	    ffelex_card_image_[i] = ' ';
	  ffelex_card_image_[i] = '\0';
	  ffelex_card_length_ = i;
	  c = ' ';
	}

      switch (ffelex_raw_mode_)
	{
	case -3:
	  c = ffelex_backslash_ (c, column);
	  if (c == EOF)
	    break;

	  if (!ffelex_backslash_reconsider_)
	    ffelex_append_to_token_ (c);
	  ffelex_raw_mode_ = -1;
	  break;

	case -2:
	  if (c == ffelex_raw_char_)
	    {
	      ffelex_raw_mode_ = -1;
	      ffelex_append_to_token_ (c);
	    }
	  else
	    {
	      ffelex_raw_mode_ = 0;
	      ffelex_backslash_reconsider_ = TRUE;
	    }
	  break;

	case -1:
	  if (c == ffelex_raw_char_)
	    ffelex_raw_mode_ = -2;
	  else
	    {
	      c = ffelex_backslash_ (c, column);
	      if (c == EOF)
		{
		  ffelex_raw_mode_ = -3;
		  break;
		}

	      ffelex_append_to_token_ (c);
	    }
	  break;

	default:
	  c = ffelex_backslash_ (c, column);
	  if (c == EOF)
	    break;

	  if (!ffelex_backslash_reconsider_)
	    {
	      ffelex_append_to_token_ (c);
	      --ffelex_raw_mode_;
	    }
	  break;
	}

      if (ffelex_backslash_reconsider_)
	ffelex_backslash_reconsider_ = FALSE;
      else
	c = ffelex_card_image_[++column];

      if (ffelex_raw_mode_ == 0)
	{
	  ffelex_send_token_ ();
	  assert (ffelex_raw_mode_ == 0);
	  while (c == ' ')
	    c = ffelex_card_image_[++column];
	  if ((c == '\0')
	      || (c == '!')
	      || ((c == '/')
		  && (ffelex_card_image_[column + 1] == '*')))
	    goto beginning_of_line;	/* :::::::::::::::::::: */
	  goto parse_nonraw_character;	/* :::::::::::::::::::: */
	}
      goto parse_raw_character;	/* :::::::::::::::::::: */
    }

 parse_nonraw_character:	/* :::::::::::::::::::: */

  switch (ffelex_token_->type)
    {
    case FFELEX_typeNONE:
      switch (c)
	{
	case '\"':
	  ffelex_token_->type = FFELEX_typeQUOTE;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '$':
	  ffelex_token_->type = FFELEX_typeDOLLAR;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '%':
	  ffelex_token_->type = FFELEX_typePERCENT;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '&':
	  ffelex_token_->type = FFELEX_typeAMPERSAND;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '\'':
	  ffelex_token_->type = FFELEX_typeAPOSTROPHE;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '(':
	  ffelex_token_->type = FFELEX_typeOPEN_PAREN;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case ')':
	  ffelex_token_->type = FFELEX_typeCLOSE_PAREN;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '*':
	  ffelex_token_->type = FFELEX_typeASTERISK;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '+':
	  ffelex_token_->type = FFELEX_typePLUS;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case ',':
	  ffelex_token_->type = FFELEX_typeCOMMA;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '-':
	  ffelex_token_->type = FFELEX_typeMINUS;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '.':
	  ffelex_token_->type = FFELEX_typePERIOD;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '/':
	  ffelex_token_->type = FFELEX_typeSLASH;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  ffelex_token_->type
	    = ffelex_hexnum_ ? FFELEX_typeNAME : FFELEX_typeNUMBER;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_append_to_token_ (c);
	  break;

	case ':':
	  ffelex_token_->type = FFELEX_typeCOLON;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case ';':
	  ffelex_token_->type = FFELEX_typeSEMICOLON;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_permit_include_ = TRUE;
	  ffelex_send_token_ ();
	  ffelex_permit_include_ = FALSE;
	  break;

	case '<':
	  ffelex_token_->type = FFELEX_typeOPEN_ANGLE;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '=':
	  ffelex_token_->type = FFELEX_typeEQUALS;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '>':
	  ffelex_token_->type = FFELEX_typeCLOSE_ANGLE;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '?':
	  ffelex_token_->type = FFELEX_typeQUESTION;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '_':
	  if (1 || ffe_is_90 ())
	    {
	      ffelex_token_->type = FFELEX_typeUNDERSCORE;
	      ffelex_token_->where_line
		= ffewhere_line_use (ffelex_current_wl_);
	      ffelex_token_->where_col
		= ffewhere_column_new (column + 1);
	      ffelex_send_token_ ();
	      break;
	    }
	  /* Fall through. */
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	  c = ffesrc_char_source (c);

	  if (ffesrc_char_match_init (c, 'H', 'h')
	      && ffelex_expecting_hollerith_ != 0)
	    {
	      ffelex_raw_mode_ = ffelex_expecting_hollerith_;
	      ffelex_token_->type = FFELEX_typeHOLLERITH;
	      ffelex_token_->where_line = ffelex_raw_where_line_;
	      ffelex_token_->where_col = ffelex_raw_where_col_;
	      ffelex_raw_where_line_ = ffewhere_line_unknown ();
	      ffelex_raw_where_col_ = ffewhere_column_unknown ();
	      c = ffelex_card_image_[++column];
	      goto parse_raw_character;	/* :::::::::::::::::::: */
	    }

	  if (ffelex_names_)
	    {
	      ffelex_token_->where_line
		= ffewhere_line_use (ffelex_token_->currentnames_line
				     = ffewhere_line_use (ffelex_current_wl_));
	      ffelex_token_->where_col
		= ffewhere_column_use (ffelex_token_->currentnames_col
				       = ffewhere_column_new (column + 1));
	      ffelex_token_->type = FFELEX_typeNAMES;
	    }
	  else
	    {
	      ffelex_token_->where_line
		= ffewhere_line_use (ffelex_current_wl_);
	      ffelex_token_->where_col = ffewhere_column_new (column + 1);
	      ffelex_token_->type = FFELEX_typeNAME;
	    }
	  ffelex_append_to_token_ (c);
	  break;

	default:
	  ffelex_bad_1_ (FFEBAD_UNRECOGNIZED_CHARACTER,
			 ffelex_linecount_current_, column + 1);
	  ffelex_finish_statement_ ();
	  disallow_continuation_line = TRUE;
	  ignore_disallowed_continuation = TRUE;
	  goto beginning_of_line_again;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeNAME:
      switch (c)
	{
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	  c = ffesrc_char_source (c);
	  /* Fall through.  */
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case '_':
	case '$':
	  if ((c == '$')
	      && !ffe_is_dollar_ok ())
	    {
	      ffelex_send_token_ ();
	      goto parse_next_character;	/* :::::::::::::::::::: */
	    }
	  ffelex_append_to_token_ (c);
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeNAMES:
      switch (c)
	{
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	  c = ffesrc_char_source (c);
	  /* Fall through.  */
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case '_':
	case '$':
	  if ((c == '$')
	      && !ffe_is_dollar_ok ())
	    {
	      ffelex_send_token_ ();
	      goto parse_next_character;	/* :::::::::::::::::::: */
	    }
	  if (ffelex_token_->length < FFEWHERE_indexMAX)
	    {
	      ffewhere_track (&ffelex_token_->currentnames_line,
			      &ffelex_token_->currentnames_col,
			      ffelex_token_->wheretrack,
			      ffelex_token_->length,
			      ffelex_linecount_current_,
			      column + 1);
	    }
	  ffelex_append_to_token_ (c);
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeNUMBER:
      switch (c)
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  ffelex_append_to_token_ (c);
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeASTERISK:
      switch (c)
	{
	case '*':		/* ** */
	  ffelex_token_->type = FFELEX_typePOWER;
	  ffelex_send_token_ ();
	  break;

	default:		/* * not followed by another *. */
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeCOLON:
      switch (c)
	{
	case ':':		/* :: */
	  ffelex_token_->type = FFELEX_typeCOLONCOLON;
	  ffelex_send_token_ ();
	  break;

	default:		/* : not followed by another :. */
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeSLASH:
      switch (c)
	{
	case '/':		/* // */
	  ffelex_token_->type = FFELEX_typeCONCAT;
	  ffelex_send_token_ ();
	  break;

	case ')':		/* /) */
	  ffelex_token_->type = FFELEX_typeCLOSE_ARRAY;
	  ffelex_send_token_ ();
	  break;

	case '=':		/* /= */
	  ffelex_token_->type = FFELEX_typeREL_NE;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeOPEN_PAREN:
      switch (c)
	{
	case '/':		/* (/ */
	  ffelex_token_->type = FFELEX_typeOPEN_ARRAY;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeOPEN_ANGLE:
      switch (c)
	{
	case '=':		/* <= */
	  ffelex_token_->type = FFELEX_typeREL_LE;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeEQUALS:
      switch (c)
	{
	case '=':		/* == */
	  ffelex_token_->type = FFELEX_typeREL_EQ;
	  ffelex_send_token_ ();
	  break;

	case '>':		/* => */
	  ffelex_token_->type = FFELEX_typePOINTS;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeCLOSE_ANGLE:
      switch (c)
	{
	case '=':		/* >= */
	  ffelex_token_->type = FFELEX_typeREL_GE;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    default:
      assert ("Serious error!!" == NULL);
      abort ();
      break;
    }

  c = ffelex_card_image_[++column];

 parse_next_character:		/* :::::::::::::::::::: */

  if (ffelex_raw_mode_ != 0)
    goto parse_raw_character;	/* :::::::::::::::::::: */

  while (c == ' ')
    c = ffelex_card_image_[++column];

  if ((c == '\0')
      || (c == '!')
      || ((c == '/')
	  && (ffelex_card_image_[column + 1] == '*')))
    {
      if ((ffelex_number_of_tokens_ == ffelex_label_tokens_)
	  && (ffelex_token_->type == FFELEX_typeNAMES)
	  && (ffelex_token_->length == 3)
	  && (ffesrc_strncmp_2c (ffe_case_match (),
				 ffelex_token_->text,
				 "END", "end", "End",
				 3)
	   == 0))
	{
	  ffelex_finish_statement_ ();
	  disallow_continuation_line = TRUE;
	  ignore_disallowed_continuation = FALSE;
	  goto beginning_of_line_again;	/* :::::::::::::::::::: */
	}
      goto beginning_of_line;	/* :::::::::::::::::::: */
    }
  goto parse_nonraw_character;	/* :::::::::::::::::::: */
}

/* ffelex_file_free -- Lex a given file in free source form

   ffewhere wf;
   FILE *f;
   ffelex_file_free(wf,f);

   Lexes the file according to Fortran 90 ANSI + VXT specifications.  */

ffelexHandler
ffelex_file_free (ffewhereFile wf, FILE *f)
{
  register int c = 0;		/* Character currently under consideration. */
  register ffewhereColumnNumber column = 0;	/* Not really; 0 means column 1... */
  bool continuation_line = FALSE;
  ffewhereColumnNumber continuation_column;
  int latest_char_in_file = 0;	/* For getting back into comment-skipping
				   code. */

  /* Lex is called for a particular file, not for a particular program unit.
     Yet the two events do share common characteristics.  The first line in a
     file or in a program unit cannot be a continuation line.  No token can
     be in mid-formation.  No current label for the statement exists, since
     there is no current statement. */

  assert (ffelex_handler_ != NULL);

  lineno = 0;
  input_filename = ffewhere_file_name (wf);
  ffelex_current_wf_ = wf;
  continuation_line = FALSE;
  ffelex_token_->type = FFELEX_typeNONE;
  ffelex_number_of_tokens_ = 0;
  ffelex_current_wl_ = ffewhere_line_unknown ();
  ffelex_current_wc_ = ffewhere_column_unknown ();
  latest_char_in_file = '\n';

  /* Come here to get a new line. */

 beginning_of_line:		/* :::::::::::::::::::: */

  c = latest_char_in_file;
  if ((c == EOF) || ((c = ffelex_getc_ (f)) == EOF))
    {

     end_of_file:		/* :::::::::::::::::::: */

      /* Line ending in EOF instead of \n still counts as a whole line. */

      ffelex_finish_statement_ ();
      ffewhere_line_kill (ffelex_current_wl_);
      ffewhere_column_kill (ffelex_current_wc_);
      return (ffelexHandler) ffelex_handler_;
    }

  ffelex_next_line_ ();

  ffelex_bad_line_ = FALSE;

  /* Skip over initial-comment and empty lines as quickly as possible! */

  while ((c == '\n')
	 || (c == '!')
	 || (c == '#'))
    {
      if (c == '#')
	c = ffelex_hash_ (f);

     comment_line:		/* :::::::::::::::::::: */

      while ((c != '\n') && (c != EOF))
	c = getc (f);

      if (c == EOF)
	{
	  ffelex_next_line_ ();
	  goto end_of_file;	/* :::::::::::::::::::: */
	}

      c = getc (f);

      ffelex_next_line_ ();

      if (c == EOF)
	goto end_of_file;	/* :::::::::::::::::::: */
    }

  ffelex_saw_tab_ = FALSE;

  column = ffelex_image_char_ (c, 0);

  /* Read the entire line in as is (with whitespace processing).  */

  while (((c = getc (f)) != '\n') && (c != EOF))
    column = ffelex_image_char_ (c, column);

  if (ffelex_bad_line_)
    {
      ffelex_card_image_[column] = '\0';
      ffelex_card_length_ = column;
      goto comment_line;		/* :::::::::::::::::::: */
    }

  /* If no tab, cut off line after column 132.  */

  if (!ffelex_saw_tab_ && (column > FFELEX_FREE_MAX_COLUMNS_))
    column = FFELEX_FREE_MAX_COLUMNS_;

  ffelex_card_image_[column] = '\0';
  ffelex_card_length_ = column;

  /* Save next char in file so we can use register-based c while analyzing
     line we just read. */

  latest_char_in_file = c;	/* Should be either '\n' or EOF. */

  column = 0;
  continuation_column = 0;

  /* Skip over initial spaces to see if the first nonblank character
     is exclamation point, newline, or EOF (line is therefore a comment) or
     ampersand (line is therefore a continuation line). */

  while ((c = ffelex_card_image_[column]) == ' ')
    ++column;

  switch (c)
    {
    case '!':
    case '\0':
      goto beginning_of_line;	/* :::::::::::::::::::: */

    case '&':
      continuation_column = column + 1;
      break;

    default:
      break;
    }

  /* The line definitely has content of some kind, install new end-statement
     point for error messages. */

  ffewhere_line_kill (ffelex_current_wl_);
  ffewhere_column_kill (ffelex_current_wc_);
  ffelex_current_wl_ = ffewhere_line_new (ffelex_linecount_current_);
  ffelex_current_wc_ = ffewhere_column_new (ffelex_card_length_ + 1);

  /* Figure out which column to start parsing at. */

  if (continuation_line)
    {
      if (continuation_column == 0)
	{
	  if (ffelex_raw_mode_ != 0)
	    {
	      ffelex_bad_1_ (FFEBAD_BAD_CHAR_CONTINUE,
			     ffelex_linecount_current_, column + 1);
	    }
	  else if (ffelex_token_->type != FFELEX_typeNONE)
	    {
	      ffelex_bad_1_ (FFEBAD_BAD_LEXTOK_CONTINUE,
			     ffelex_linecount_current_, column + 1);
	    }
	}
      else if (ffelex_is_free_char_ctx_contin_ (continuation_column))
	{			/* Line contains only a single "&" as only
				   nonblank character. */
	  ffelex_bad_1_ (FFEBAD_BAD_FREE_CONTINUE,
			 ffelex_linecount_current_, continuation_column);
	  goto beginning_of_line;	/* :::::::::::::::::::: */
	}
      column = continuation_column;
    }
  else
    column = 0;

  c = ffelex_card_image_[column];
  continuation_line = FALSE;

  /* Here is the main engine for parsing.  c holds the character at column.
     It is already known that c is not a blank, end of line, or shriek,
     unless ffelex_raw_mode_ is not 0 (indicating we are in a
     character/hollerith constant).  A partially filled token may already
     exist in ffelex_token_. */

  if (ffelex_raw_mode_ != 0)
    {

    parse_raw_character:	/* :::::::::::::::::::: */

      switch (c)
	{
	case '&':
	  if (ffelex_is_free_char_ctx_contin_ (column + 1))
	    {
	      continuation_line = TRUE;
	      goto beginning_of_line;	/* :::::::::::::::::::: */
	    }
	  break;

	case '\0':
	  ffelex_finish_statement_ ();
	  goto beginning_of_line;	/* :::::::::::::::::::: */

	default:
	  break;
	}

      switch (ffelex_raw_mode_)
	{
	case -3:
	  c = ffelex_backslash_ (c, column);
	  if (c == EOF)
	    break;

	  if (!ffelex_backslash_reconsider_)
	    ffelex_append_to_token_ (c);
	  ffelex_raw_mode_ = -1;
	  break;

	case -2:
	  if (c == ffelex_raw_char_)
	    {
	      ffelex_raw_mode_ = -1;
	      ffelex_append_to_token_ (c);
	    }
	  else
	    {
	      ffelex_raw_mode_ = 0;
	      ffelex_backslash_reconsider_ = TRUE;
	    }
	  break;

	case -1:
	  if (c == ffelex_raw_char_)
	    ffelex_raw_mode_ = -2;
	  else
	    {
	      c = ffelex_backslash_ (c, column);
	      if (c == EOF)
		{
		  ffelex_raw_mode_ = -3;
		  break;
		}

	      ffelex_append_to_token_ (c);
	    }
	  break;

	default:
	  c = ffelex_backslash_ (c, column);
	  if (c == EOF)
	    break;

	  if (!ffelex_backslash_reconsider_)
	    {
	      ffelex_append_to_token_ (c);
	      --ffelex_raw_mode_;
	    }
	  break;
	}

      if (ffelex_backslash_reconsider_)
	ffelex_backslash_reconsider_ = FALSE;
      else
	c = ffelex_card_image_[++column];

      if (ffelex_raw_mode_ == 0)
	{
	  ffelex_send_token_ ();
	  assert (ffelex_raw_mode_ == 0);
	  while (c == ' ')
	    c = ffelex_card_image_[++column];
	  if ((c == '\0') || (c == '!'))
	    {
	      ffelex_finish_statement_ ();
	      goto beginning_of_line;	/* :::::::::::::::::::: */
	    }
	  if ((c == '&') && ffelex_is_free_nonc_ctx_contin_ (column + 1))
	    {
	      continuation_line = TRUE;
	      goto beginning_of_line;	/* :::::::::::::::::::: */
	    }
	  goto parse_nonraw_character_noncontin;	/* :::::::::::::::::::: */
	}
      goto parse_raw_character;	/* :::::::::::::::::::: */
    }

 parse_nonraw_character:	/* :::::::::::::::::::: */

  if ((c == '&') && ffelex_is_free_nonc_ctx_contin_ (column + 1))
    {
      continuation_line = TRUE;
      goto beginning_of_line;	/* :::::::::::::::::::: */
    }

 parse_nonraw_character_noncontin:	/* :::::::::::::::::::: */

  switch (ffelex_token_->type)
    {
    case FFELEX_typeNONE:
      if (c == ' ')
	{			/* Otherwise
				   finish-statement/continue-statement
				   already checked. */
	  while (c == ' ')
	    c = ffelex_card_image_[++column];
	  if ((c == '\0') || (c == '!'))
	    {
	      ffelex_finish_statement_ ();
	      goto beginning_of_line;	/* :::::::::::::::::::: */
	    }
	  if ((c == '&') && ffelex_is_free_nonc_ctx_contin_ (column + 1))
	    {
	      continuation_line = TRUE;
	      goto beginning_of_line;	/* :::::::::::::::::::: */
	    }
	}

      switch (c)
	{
	case '\"':
	  ffelex_token_->type = FFELEX_typeQUOTE;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '$':
	  ffelex_token_->type = FFELEX_typeDOLLAR;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '%':
	  ffelex_token_->type = FFELEX_typePERCENT;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '&':
	  ffelex_token_->type = FFELEX_typeAMPERSAND;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '\'':
	  ffelex_token_->type = FFELEX_typeAPOSTROPHE;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '(':
	  ffelex_token_->type = FFELEX_typeOPEN_PAREN;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case ')':
	  ffelex_token_->type = FFELEX_typeCLOSE_PAREN;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '*':
	  ffelex_token_->type = FFELEX_typeASTERISK;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '+':
	  ffelex_token_->type = FFELEX_typePLUS;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case ',':
	  ffelex_token_->type = FFELEX_typeCOMMA;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '-':
	  ffelex_token_->type = FFELEX_typeMINUS;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '.':
	  ffelex_token_->type = FFELEX_typePERIOD;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '/':
	  ffelex_token_->type = FFELEX_typeSLASH;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  ffelex_token_->type
	    = ffelex_hexnum_ ? FFELEX_typeNAME : FFELEX_typeNUMBER;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_append_to_token_ (c);
	  break;

	case ':':
	  ffelex_token_->type = FFELEX_typeCOLON;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case ';':
	  ffelex_token_->type = FFELEX_typeSEMICOLON;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_permit_include_ = TRUE;
	  ffelex_send_token_ ();
	  ffelex_permit_include_ = FALSE;
	  break;

	case '<':
	  ffelex_token_->type = FFELEX_typeOPEN_ANGLE;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '=':
	  ffelex_token_->type = FFELEX_typeEQUALS;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '>':
	  ffelex_token_->type = FFELEX_typeCLOSE_ANGLE;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  break;

	case '?':
	  ffelex_token_->type = FFELEX_typeQUESTION;
	  ffelex_token_->where_line = ffewhere_line_use (ffelex_current_wl_);
	  ffelex_token_->where_col = ffewhere_column_new (column + 1);
	  ffelex_send_token_ ();
	  break;

	case '_':
	  if (1 || ffe_is_90 ())
	    {
	      ffelex_token_->type = FFELEX_typeUNDERSCORE;
	      ffelex_token_->where_line
		= ffewhere_line_use (ffelex_current_wl_);
	      ffelex_token_->where_col
		= ffewhere_column_new (column + 1);
	      ffelex_send_token_ ();
	      break;
	    }
	  /* Fall through. */
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	  c = ffesrc_char_source (c);

	  if (ffesrc_char_match_init (c, 'H', 'h')
	      && ffelex_expecting_hollerith_ != 0)
	    {
	      ffelex_raw_mode_ = ffelex_expecting_hollerith_;
	      ffelex_token_->type = FFELEX_typeHOLLERITH;
	      ffelex_token_->where_line = ffelex_raw_where_line_;
	      ffelex_token_->where_col = ffelex_raw_where_col_;
	      ffelex_raw_where_line_ = ffewhere_line_unknown ();
	      ffelex_raw_where_col_ = ffewhere_column_unknown ();
	      c = ffelex_card_image_[++column];
	      goto parse_raw_character;	/* :::::::::::::::::::: */
	    }

	  if (ffelex_names_pure_)
	    {
	      ffelex_token_->where_line
		= ffewhere_line_use (ffelex_token_->currentnames_line
				     = ffewhere_line_use (ffelex_current_wl_));
	      ffelex_token_->where_col
		= ffewhere_column_use (ffelex_token_->currentnames_col
				       = ffewhere_column_new (column + 1));
	      ffelex_token_->type = FFELEX_typeNAMES;
	    }
	  else
	    {
	      ffelex_token_->where_line
		= ffewhere_line_use (ffelex_current_wl_);
	      ffelex_token_->where_col = ffewhere_column_new (column + 1);
	      ffelex_token_->type = FFELEX_typeNAME;
	    }
	  ffelex_append_to_token_ (c);
	  break;

	default:
	  ffelex_bad_1_ (FFEBAD_UNRECOGNIZED_CHARACTER,
			 ffelex_linecount_current_, column + 1);
	  ffelex_finish_statement_ ();
	  goto beginning_of_line;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeNAME:
      switch (c)
	{
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	  c = ffesrc_char_source (c);
	  /* Fall through.  */
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case '_':
	case '$':
	  if ((c == '$')
	      && !ffe_is_dollar_ok ())
	    {
	      ffelex_send_token_ ();
	      goto parse_next_character;	/* :::::::::::::::::::: */
	    }
	  ffelex_append_to_token_ (c);
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeNAMES:
      switch (c)
	{
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	  c = ffesrc_char_source (c);
	  /* Fall through.  */
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case '_':
	case '$':
	  if ((c == '$')
	      && !ffe_is_dollar_ok ())
	    {
	      ffelex_send_token_ ();
	      goto parse_next_character;	/* :::::::::::::::::::: */
	    }
	  if (ffelex_token_->length < FFEWHERE_indexMAX)
	    {
	      ffewhere_track (&ffelex_token_->currentnames_line,
			      &ffelex_token_->currentnames_col,
			      ffelex_token_->wheretrack,
			      ffelex_token_->length,
			      ffelex_linecount_current_,
			      column + 1);
	    }
	  ffelex_append_to_token_ (c);
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeNUMBER:
      switch (c)
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	  ffelex_append_to_token_ (c);
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeASTERISK:
      switch (c)
	{
	case '*':		/* ** */
	  ffelex_token_->type = FFELEX_typePOWER;
	  ffelex_send_token_ ();
	  break;

	default:		/* * not followed by another *. */
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeCOLON:
      switch (c)
	{
	case ':':		/* :: */
	  ffelex_token_->type = FFELEX_typeCOLONCOLON;
	  ffelex_send_token_ ();
	  break;

	default:		/* : not followed by another :. */
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeSLASH:
      switch (c)
	{
	case '/':		/* // */
	  ffelex_token_->type = FFELEX_typeCONCAT;
	  ffelex_send_token_ ();
	  break;

	case ')':		/* /) */
	  ffelex_token_->type = FFELEX_typeCLOSE_ARRAY;
	  ffelex_send_token_ ();
	  break;

	case '=':		/* /= */
	  ffelex_token_->type = FFELEX_typeREL_NE;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeOPEN_PAREN:
      switch (c)
	{
	case '/':		/* (/ */
	  ffelex_token_->type = FFELEX_typeOPEN_ARRAY;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeOPEN_ANGLE:
      switch (c)
	{
	case '=':		/* <= */
	  ffelex_token_->type = FFELEX_typeREL_LE;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeEQUALS:
      switch (c)
	{
	case '=':		/* == */
	  ffelex_token_->type = FFELEX_typeREL_EQ;
	  ffelex_send_token_ ();
	  break;

	case '>':		/* => */
	  ffelex_token_->type = FFELEX_typePOINTS;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    case FFELEX_typeCLOSE_ANGLE:
      switch (c)
	{
	case '=':		/* >= */
	  ffelex_token_->type = FFELEX_typeREL_GE;
	  ffelex_send_token_ ();
	  break;

	default:
	  ffelex_send_token_ ();
	  goto parse_next_character;	/* :::::::::::::::::::: */
	}
      break;

    default:
      assert ("Serious error!" == NULL);
      abort ();
      break;
    }

  c = ffelex_card_image_[++column];

 parse_next_character:		/* :::::::::::::::::::: */

  if (ffelex_raw_mode_ != 0)
    goto parse_raw_character;	/* :::::::::::::::::::: */

  if ((c == '\0') || (c == '!'))
    {
      ffelex_finish_statement_ ();
      goto beginning_of_line;	/* :::::::::::::::::::: */
    }
  goto parse_nonraw_character;	/* :::::::::::::::::::: */
}

/* See the code in com.c that calls this to understand why.  */

void
ffelex_hash_kludge (FILE *finput)
{
  /* If you change this constant string, you have to change whatever
     code might thus be affected by it in terms of having to use
     ffelex_getc_() instead of getc() in the lexers and _hash_.  */
  static const char match[] = "# 1 \"";
  static int kludge[ARRAY_SIZE (match) + 1];
  int c;
  const char *p;
  int *q;

  /* Read chars as long as they match the target string.
     Copy them into an array that will serve as a record
     of what we read (essentially a multi-char ungetc(),
     for code that uses ffelex_getc_ instead of getc() elsewhere
     in the lexer.  */
  for (p = &match[0], q = &kludge[0], c = getc (finput);
       (c == *p) && (*p != '\0') && (c != EOF);
       ++p, ++q, c = getc (finput))
    *q = c;

  *q = c;			/* Might be EOF, which requires int. */
  *++q = 0;

  ffelex_kludge_chars_ = &kludge[0];

  if (*p == 0)
    {
      ffelex_kludge_flag_ = TRUE;
      ++ffelex_kludge_chars_;
      ffelex_hash_ (finput);	/* Handle it NOW rather than later. */
      ffelex_kludge_flag_ = FALSE;
    }
}

void
ffelex_init_1 ()
{
  unsigned int i;

  ffelex_final_nontab_column_ = ffe_fixed_line_length ();
  ffelex_card_size_ = FFELEX_columnINITIAL_SIZE_;
  ffelex_card_image_ = malloc_new_ksr (malloc_pool_image (),
				       "FFELEX card image",
				       FFELEX_columnINITIAL_SIZE_ + 9);
  ffelex_card_image_[0] = '\0';

  for (i = 0; i < 256; ++i)
    ffelex_first_char_[i] = FFELEX_typeERROR;

  ffelex_first_char_['\t'] = FFELEX_typeRAW;
  ffelex_first_char_['\n'] = FFELEX_typeCOMMENT;
  ffelex_first_char_['\v'] = FFELEX_typeCOMMENT;
  ffelex_first_char_['\f'] = FFELEX_typeCOMMENT;
  ffelex_first_char_['\r'] = FFELEX_typeRAW;
  ffelex_first_char_[' '] = FFELEX_typeRAW;
  ffelex_first_char_['!'] = FFELEX_typeCOMMENT;
  ffelex_first_char_['*'] = FFELEX_typeCOMMENT;
  ffelex_first_char_['/'] = FFELEX_typeSLASH;
  ffelex_first_char_['&'] = FFELEX_typeRAW;
  ffelex_first_char_['#'] = FFELEX_typeHASH;

  for (i = '0'; i <= '9'; ++i)
    ffelex_first_char_[i] = FFELEX_typeRAW;

  if ((ffe_case_match () == FFE_caseNONE)
      || ((ffe_case_match () == FFE_caseUPPER)
	  && (ffe_case_source () != FFE_caseLOWER))	/* Idiot!  :-) */
      || ((ffe_case_match () == FFE_caseLOWER)
	  && (ffe_case_source () == FFE_caseLOWER)))
    {
      ffelex_first_char_['C'] = FFELEX_typeCOMMENT;
      ffelex_first_char_['D'] = FFELEX_typeCOMMENT;
    }
  if ((ffe_case_match () == FFE_caseNONE)
      || ((ffe_case_match () == FFE_caseLOWER)
	  && (ffe_case_source () != FFE_caseUPPER))	/* Idiot!  :-) */
      || ((ffe_case_match () == FFE_caseUPPER)
	  && (ffe_case_source () == FFE_caseUPPER)))
    {
      ffelex_first_char_['c'] = FFELEX_typeCOMMENT;
      ffelex_first_char_['d'] = FFELEX_typeCOMMENT;
    }

  ffelex_linecount_current_ = 0;
  ffelex_linecount_next_ = 1;
  ffelex_raw_mode_ = 0;
  ffelex_set_include_ = FALSE;
  ffelex_permit_include_ = FALSE;
  ffelex_names_ = TRUE;		/* First token in program is a names. */
  ffelex_names_pure_ = FALSE;	/* Free-form lexer does NAMES only for
				   FORMAT. */
  ffelex_hexnum_ = FALSE;
  ffelex_expecting_hollerith_ = 0;
  ffelex_raw_where_line_ = ffewhere_line_unknown ();
  ffelex_raw_where_col_ = ffewhere_column_unknown ();

  ffelex_token_ = ffelex_token_new_ ();
  ffelex_token_->type = FFELEX_typeNONE;
  ffelex_token_->uses = 1;
  ffelex_token_->where_line = ffewhere_line_unknown ();
  ffelex_token_->where_col = ffewhere_column_unknown ();
  ffelex_token_->text = NULL;

  ffelex_handler_ = NULL;
}

/* ffelex_is_names_expected -- Is the current parser expecting NAMES vs. NAME?

   if (ffelex_is_names_expected())
       // Deliver NAMES token
     else
       // Deliver NAME token

   Must be called while lexer is active, obviously.  */

bool
ffelex_is_names_expected ()
{
  return ffelex_names_;
}

/* Current card image, which has the master linecount number
   ffelex_linecount_current_.  */

char *
ffelex_line ()
{
  return ffelex_card_image_;
}

/* ffelex_line_length -- Return length of current lexer line

   printf("Length is %lu\n",ffelex_line_length());

   Must be called while lexer is active, obviously.  */

ffewhereColumnNumber
ffelex_line_length ()
{
  return ffelex_card_length_;
}

/* Master line count of current card image, or 0 if no card image
   is current.  */

ffewhereLineNumber
ffelex_line_number ()
{
  return ffelex_linecount_current_;
}

/* ffelex_set_expecting_hollerith -- Set hollerith expectation status

   ffelex_set_expecting_hollerith(0);

   Lex initially assumes no hollerith constant is about to show up.  If
   syntactic analysis expects one, it should call this function with the
   number of characters expected in the constant immediately after recognizing
   the decimal number preceding the "H" and the constant itself.  Then, if
   the next character is indeed H, the lexer will interpret it as beginning
   a hollerith constant and ship the token formed by reading the specified
   number of characters (interpreting blanks and otherwise-comments too)
   from the input file.	 It is up to syntactic analysis to call this routine
   again with 0 to turn hollerith detection off immediately upon receiving
   the token that might or might not be HOLLERITH.

   Also call this after seeing an APOSTROPHE or QUOTE token that begins a
   character constant.	Pass the expected termination character (apostrophe
   or quote).

   Pass for length either the length of the hollerith (must be > 0), -1
   meaning expecting a character constant, or 0 to cancel expectation of
   a hollerith only after calling it with a length of > 0 and receiving the
   next token (which may or may not have been a HOLLERITH token).

   Pass for which either an apostrophe or quote when passing length of -1.
   Else which is a don't-care.

   Pass for line and column the line/column info for the token beginning the
   character or hollerith constant, for use in error messages, when passing
   a length of -1 -- this function will invoke ffewhere_line/column_use to
   make its own copies.	 Else line and column are don't-cares (when length
   is 0) and the outstanding copies of the previous line/column info, if
   still around, are killed.

   21-Feb-90  JCB  3.1
      When called with length of 0, also zero ffelex_raw_mode_.	 This is
      so ffest_save_ can undo the effects of replaying tokens like
      APOSTROPHE and QUOTE.
   25-Jan-90  JCB  3.0
      New line, column arguments allow error messages to point to the true
      beginning of a character/hollerith constant, rather than the beginning
      of the content part, which makes them more consistent and helpful.
   05-Nov-89  JCB  2.0
      New "which" argument allows caller to specify termination character,
      which should be apostrophe or double-quote, to support Fortran 90.  */

void
ffelex_set_expecting_hollerith (long length, char which,
				ffewhereLine line, ffewhereColumn column)
{

  /* First kill the pending line/col info, if any (should only be pending
     when this call has length==0, the previous call had length>0, and a
     non-HOLLERITH token was sent in between the calls, but play it safe). */

  ffewhere_line_kill (ffelex_raw_where_line_);
  ffewhere_column_kill (ffelex_raw_where_col_);

  /* Now handle the length function. */
  switch (length)
    {
    case 0:
      ffelex_expecting_hollerith_ = 0;
      ffelex_raw_mode_ = 0;
      ffelex_raw_where_line_ = ffewhere_line_unknown ();
      ffelex_raw_where_col_ = ffewhere_column_unknown ();
      return;			/* Don't set new line/column info from args. */

    case -1:
      ffelex_raw_mode_ = -1;
      ffelex_raw_char_ = which;
      break;

    default:			/* length > 0 */
      ffelex_expecting_hollerith_ = length;
      break;
    }

  /* Now set new line/column information from passed args. */

  ffelex_raw_where_line_ = ffewhere_line_use (line);
  ffelex_raw_where_col_ = ffewhere_column_use (column);
}

/* ffelex_set_handler -- Set handler for tokens before calling _fixed or _free

   ffelex_set_handler((ffelexHandler) my_first_handler);

   Must be called before calling ffelex_file_fixed or ffelex_file_free or
   after they return, but not while they are active.  */

void
ffelex_set_handler (ffelexHandler first)
{
  ffelex_handler_ = first;
}

/* ffelex_set_hexnum -- Set hexnum flag

   ffelex_set_hexnum(TRUE);

   Lex normally interprets a token starting with [0-9] as a NUMBER token,
   so if it sees a [A-Za-z] in it, it stops parsing the NUMBER and leaves
   the character as the first of the next token.  But when parsing a
   hexadecimal number, by calling this function with TRUE before starting
   the parse of the token itself, lex will interpret [0-9] as the start
   of a NAME token.  */

void
ffelex_set_hexnum (bool f)
{
  ffelex_hexnum_ = f;
}

/* ffelex_set_include -- Set INCLUDE file to be processed next

   ffewhereFile wf;  // The ffewhereFile object for the file.
   bool free_form;  // TRUE means read free-form file, FALSE fixed-form.
   FILE *fi;  // The file to INCLUDE.
   ffelex_set_include(wf,free_form,fi);

   Must be called only after receiving the EOS token following a valid
   INCLUDE statement specifying a file that has already been successfully
   opened.  */

void
ffelex_set_include (ffewhereFile wf, bool free_form, FILE *fi)
{
  assert (ffelex_permit_include_);
  assert (!ffelex_set_include_);
  ffelex_set_include_ = TRUE;
  ffelex_include_free_form_ = free_form;
  ffelex_include_file_ = fi;
  ffelex_include_wherefile_ = wf;
}

/* ffelex_set_names -- Set names/name flag, names = TRUE

   ffelex_set_names(FALSE);

   Lex initially assumes multiple names should be formed.  If this function is
   called with FALSE, then single names are formed instead.  The differences
   are a difference in the token type (FFELEX_typeNAMES vs. FFELEX_typeNAME)
   and in whether full source-location tracking is performed (it is for
   multiple names, not for single names), which is more expensive in terms of
   CPU time.  */

void
ffelex_set_names (bool f)
{
  ffelex_names_ = f;
  if (!f)
    ffelex_names_pure_ = FALSE;
}

/* ffelex_set_names_pure -- Set names/name (pure) flag, names = TRUE

   ffelex_set_names_pure(FALSE);

   Like ffelex_set_names, except affects both lexers.  Normally, the
   free-form lexer need not generate NAMES tokens because adjacent NAME
   tokens must be separated by spaces which causes the lexer to generate
   separate tokens for analysis (whereas in fixed-form the spaces are
   ignored resulting in one long token).  But in FORMAT statements, for
   some reason, the Fortran 90 standard specifies that spaces can occur
   anywhere within a format-item-list with no effect on the format spec
   (except of course within character string edit descriptors), which means
   that "1PE14.2" and "1 P E 1 4 . 2" are equivalent.  For the FORMAT
   statement handling, the existence of spaces makes it hard to deal with,
   because each token is seen distinctly (i.e. seven tokens in the latter
   example).  But when no spaces are provided, as in the former example,
   then only four tokens are generated, NUMBER("1"), NAME("PE14"), PERIOD,
   NUMBER ("2").  By generating a NAMES instead of NAME, three things happen:
   One, ffest_kw_format_ does a substring rather than full-string match,
   and thus matches "PE14" to "PE"; two, ffelex_token_xyz_from_names functions
   may be used to pull NAME/NAMES and NUMBER tokens out of the NAMES token;
   and three, error reporting can point to the actual character rather than
   at or prior to it.  The first two things could be resolved by providing
   alternate functions fairly easy, thus allowing FORMAT handling to expect
   both lexers to generate NAME tokens instead of NAMES (with otherwise minor
   changes to FORMAT parsing), but the third, error reporting, would suffer,
   and when one makes mistakes in a FORMAT, believe me, one wants a pointer
   to exactly where the compilers thinks the problem is, to even begin to get
   a handle on it.  So there.  */

void
ffelex_set_names_pure (bool f)
{
  ffelex_names_pure_ = f;
  ffelex_names_ = f;
}

/* ffelex_splice_tokens -- Splice off and send tokens from a NAMES

   return (ffelexHandler) ffelex_splice_tokens(first_handler,master_token,
	 start_char_index);

   Returns first_handler if start_char_index chars into master_token (which
   must be a NAMES token) is '\0'. Else, creates a subtoken from that
   char, either NUMBER (if it is a digit), a NAME (if a valid firstnamechar),
   an UNDERSCORE (if an underscore), or DOLLAR (if a dollar sign)
   and sends it to first_handler. If anything other than NAME is sent, the
   character at the end of it in the master token is examined to see if it
   begins a NAME, NUMBER, UNDERSCORE, or DOLLAR, and, if so,
   the handler returned by first_handler is invoked with that token, and
   this process is repeated until the end of the master token or a NAME
   token is reached.  */

ffelexHandler
ffelex_splice_tokens (ffelexHandler first, ffelexToken master,
		      ffeTokenLength start)
{
  unsigned char *p;
  ffeTokenLength i;
  ffelexToken t;

  p = ffelex_token_text (master) + (i = start);

  while (*p != '\0')
    {
      if (ISDIGIT (*p))
	{
	  t = ffelex_token_number_from_names (master, i);
	  p += ffelex_token_length (t);
	  i += ffelex_token_length (t);
	}
      else if (ffesrc_is_name_init (*p))
	{
	  t = ffelex_token_name_from_names (master, i, 0);
	  p += ffelex_token_length (t);
	  i += ffelex_token_length (t);
	}
      else if (*p == '$')
	{
	  t = ffelex_token_dollar_from_names (master, i);
	  ++p;
	  ++i;
	}
      else if (*p == '_')
	{
	  t = ffelex_token_uscore_from_names (master, i);
	  ++p;
	  ++i;
	}
      else
	{
	  assert ("not a valid NAMES character" == NULL);
	  t = NULL;
	}
      assert (first != NULL);
      first = (ffelexHandler) (*first) (t);
      ffelex_token_kill (t);
    }

  return first;
}

/* ffelex_swallow_tokens -- Eat all tokens delivered to me

   return ffelex_swallow_tokens;

   Return this handler when you don't want to look at any more tokens in the
   statement because you've encountered an unrecoverable error in the
   statement.  */

ffelexHandler
ffelex_swallow_tokens (ffelexToken t, ffelexHandler handler)
{
  assert (handler != NULL);

  if ((t != NULL) && ((ffelex_token_type (t) == FFELEX_typeEOS)
		      || (ffelex_token_type (t) == FFELEX_typeSEMICOLON)))
    return (ffelexHandler) (*handler) (t);

  ffelex_eos_handler_ = handler;
  return (ffelexHandler) ffelex_swallow_tokens_;
}

/* ffelex_token_dollar_from_names -- Return a dollar from within a names token

   ffelexToken t;
   t = ffelex_token_dollar_from_names(t,6);

   It's as if you made a new token of dollar type having the dollar
   at, in the example above, the sixth character of the NAMES token.  */

ffelexToken
ffelex_token_dollar_from_names (ffelexToken t, ffeTokenLength start)
{
  ffelexToken nt;

  assert (t != NULL);
  assert (ffelex_token_type (t) == FFELEX_typeNAMES);
  assert (start < t->length);
  assert (t->text[start] == '$');

  /* Now make the token. */

  nt = ffelex_token_new_ ();
  nt->type = FFELEX_typeDOLLAR;
  nt->length = 0;
  nt->uses = 1;
  ffewhere_set_from_track (&nt->where_line, &nt->where_col, t->where_line,
			   t->where_col, t->wheretrack, start);
  nt->text = NULL;
  return nt;
}

/* ffelex_token_kill -- Decrement use count for token, kill if no uses left

   ffelexToken t;
   ffelex_token_kill(t);

   Complements a call to ffelex_token_use or ffelex_token_new_....  */

void
ffelex_token_kill (ffelexToken t)
{
  assert (t != NULL);

  assert (t->uses > 0);

  if (--t->uses != 0)
    return;

  --ffelex_total_tokens_;

  if (t->type == FFELEX_typeNAMES)
    ffewhere_track_kill (t->where_line, t->where_col,
			 t->wheretrack, t->length);
  ffewhere_line_kill (t->where_line);
  ffewhere_column_kill (t->where_col);
  if (t->text != NULL)
    malloc_kill_ksr (malloc_pool_image (), t->text, t->size + 1);
  malloc_kill_ks (malloc_pool_image (), t, sizeof (*t));
}

/* Make a new NAME token that is a substring of a NAMES token.  */

ffelexToken
ffelex_token_name_from_names (ffelexToken t, ffeTokenLength start,
			      ffeTokenLength len)
{
  ffelexToken nt;

  assert (t != NULL);
  assert (ffelex_token_type (t) == FFELEX_typeNAMES);
  assert (start < t->length);
  if (len == 0)
    len = t->length - start;
  else
    {
      assert (len > 0);
      assert ((start + len) <= t->length);
    }
  assert (ffelex_is_firstnamechar ((unsigned char)(t->text[start])));

  nt = ffelex_token_new_ ();
  nt->type = FFELEX_typeNAME;
  nt->size = len;		/* Assume nobody's gonna fiddle with token
				   text. */
  nt->length = len;
  nt->uses = 1;
  ffewhere_set_from_track (&nt->where_line, &nt->where_col, t->where_line,
			   t->where_col, t->wheretrack, start);
  nt->text = malloc_new_ksr (malloc_pool_image (), "FFELEX token text",
			     len + 1);
  strncpy (nt->text, t->text + start, len);
  nt->text[len] = '\0';
  return nt;
}

/* Make a new NAMES token that is a substring of another NAMES token.  */

ffelexToken
ffelex_token_names_from_names (ffelexToken t, ffeTokenLength start,
			       ffeTokenLength len)
{
  ffelexToken nt;

  assert (t != NULL);
  assert (ffelex_token_type (t) == FFELEX_typeNAMES);
  assert (start < t->length);
  if (len == 0)
    len = t->length - start;
  else
    {
      assert (len > 0);
      assert ((start + len) <= t->length);
    }
  assert (ffelex_is_firstnamechar ((unsigned char)(t->text[start])));

  nt = ffelex_token_new_ ();
  nt->type = FFELEX_typeNAMES;
  nt->size = len;		/* Assume nobody's gonna fiddle with token
				   text. */
  nt->length = len;
  nt->uses = 1;
  ffewhere_set_from_track (&nt->where_line, &nt->where_col, t->where_line,
			   t->where_col, t->wheretrack, start);
  ffewhere_track_copy (nt->wheretrack, t->wheretrack, start, len);
  nt->text = malloc_new_ksr (malloc_pool_image (), "FFELEX token text",
			     len + 1);
  strncpy (nt->text, t->text + start, len);
  nt->text[len] = '\0';
  return nt;
}

/* Make a new CHARACTER token.  */

ffelexToken
ffelex_token_new_character (const char *s, ffewhereLine l, ffewhereColumn c)
{
  ffelexToken t;

  t = ffelex_token_new_ ();
  t->type = FFELEX_typeCHARACTER;
  t->length = t->size = strlen (s);	/* Assume it won't get bigger. */
  t->uses = 1;
  t->text = malloc_new_ksr (malloc_pool_image (), "FFELEX token text",
			    t->size + 1);
  strcpy (t->text, s);
  t->where_line = ffewhere_line_use (l);
  t->where_col = ffewhere_column_new (c);
  return t;
}

/* Make a new EOF token right after end of file.  */

ffelexToken
ffelex_token_new_eof ()
{
  ffelexToken t;

  t = ffelex_token_new_ ();
  t->type = FFELEX_typeEOF;
  t->uses = 1;
  t->text = NULL;
  t->where_line = ffewhere_line_new (ffelex_linecount_current_);
  t->where_col = ffewhere_column_new (1);
  return t;
}

/* Make a new NAME token.  */

ffelexToken
ffelex_token_new_name (const char *s, ffewhereLine l, ffewhereColumn c)
{
  ffelexToken t;

  assert (ffelex_is_firstnamechar ((unsigned char)*s));

  t = ffelex_token_new_ ();
  t->type = FFELEX_typeNAME;
  t->length = t->size = strlen (s);	/* Assume it won't get bigger. */
  t->uses = 1;
  t->text = malloc_new_ksr (malloc_pool_image (), "FFELEX token text",
			    t->size + 1);
  strcpy (t->text, s);
  t->where_line = ffewhere_line_use (l);
  t->where_col = ffewhere_column_new (c);
  return t;
}

/* Make a new NAMES token.  */

ffelexToken
ffelex_token_new_names (const char *s, ffewhereLine l, ffewhereColumn c)
{
  ffelexToken t;

  assert (ffelex_is_firstnamechar ((unsigned char)*s));

  t = ffelex_token_new_ ();
  t->type = FFELEX_typeNAMES;
  t->length = t->size = strlen (s);	/* Assume it won't get bigger. */
  t->uses = 1;
  t->text = malloc_new_ksr (malloc_pool_image (), "FFELEX token text",
			    t->size + 1);
  strcpy (t->text, s);
  t->where_line = ffewhere_line_use (l);
  t->where_col = ffewhere_column_new (c);
  ffewhere_track_clear (t->wheretrack, t->length);	/* Assume contiguous
							   names. */
  return t;
}

/* Make a new NUMBER token.

   The first character of the string must be a digit, and only the digits
   are copied into the new number.  So this may be used to easily extract
   a NUMBER token from within any text string.  Then the length of the
   resulting token may be used to calculate where the digits stopped
   in the original string.  */

ffelexToken
ffelex_token_new_number (const char *s, ffewhereLine l, ffewhereColumn c)
{
  ffelexToken t;
  ffeTokenLength len;

  /* How long is the string of decimal digits at s? */

  len = strspn (s, "0123456789");

  /* Make sure there is at least one digit. */

  assert (len != 0);

  /* Now make the token. */

  t = ffelex_token_new_ ();
  t->type = FFELEX_typeNUMBER;
  t->length = t->size = len;	/* Assume it won't get bigger. */
  t->uses = 1;
  t->text = malloc_new_ksr (malloc_pool_image (), "FFELEX token text",
			    len + 1);
  strncpy (t->text, s, len);
  t->text[len] = '\0';
  t->where_line = ffewhere_line_use (l);
  t->where_col = ffewhere_column_new (c);
  return t;
}

/* Make a new token of any type that doesn't contain text.  A private
   function that is used by public macros in the interface file.  */

ffelexToken
ffelex_token_new_simple_ (ffelexType type, ffewhereLine l, ffewhereColumn c)
{
  ffelexToken t;

  t = ffelex_token_new_ ();
  t->type = type;
  t->uses = 1;
  t->text = NULL;
  t->where_line = ffewhere_line_use (l);
  t->where_col = ffewhere_column_new (c);
  return t;
}

/* Make a new NUMBER token from an existing NAMES token.

   Like ffelex_token_new_number, this function calculates the length
   of the digit string itself.  */

ffelexToken
ffelex_token_number_from_names (ffelexToken t, ffeTokenLength start)
{
  ffelexToken nt;
  ffeTokenLength len;

  assert (t != NULL);
  assert (ffelex_token_type (t) == FFELEX_typeNAMES);
  assert (start < t->length);

  /* How long is the string of decimal digits at s? */

  len = strspn (t->text + start, "0123456789");

  /* Make sure there is at least one digit. */

  assert (len != 0);

  /* Now make the token. */

  nt = ffelex_token_new_ ();
  nt->type = FFELEX_typeNUMBER;
  nt->size = len;		/* Assume nobody's gonna fiddle with token
				   text. */
  nt->length = len;
  nt->uses = 1;
  ffewhere_set_from_track (&nt->where_line, &nt->where_col, t->where_line,
			   t->where_col, t->wheretrack, start);
  nt->text = malloc_new_ksr (malloc_pool_image (), "FFELEX token text",
			     len + 1);
  strncpy (nt->text, t->text + start, len);
  nt->text[len] = '\0';
  return nt;
}

/* Make a new UNDERSCORE token from a NAMES token.  */

ffelexToken
ffelex_token_uscore_from_names (ffelexToken t, ffeTokenLength start)
{
  ffelexToken nt;

  assert (t != NULL);
  assert (ffelex_token_type (t) == FFELEX_typeNAMES);
  assert (start < t->length);
  assert (t->text[start] == '_');

  /* Now make the token. */

  nt = ffelex_token_new_ ();
  nt->type = FFELEX_typeUNDERSCORE;
  nt->uses = 1;
  ffewhere_set_from_track (&nt->where_line, &nt->where_col, t->where_line,
			   t->where_col, t->wheretrack, start);
  nt->text = NULL;
  return nt;
}

/* ffelex_token_use -- Return another instance of a token

   ffelexToken t;
   t = ffelex_token_use(t);

   In a sense, the new token is a copy of the old, though it might be the
   same with just a new use count.

   We use the use count method (easy).	*/

ffelexToken
ffelex_token_use (ffelexToken t)
{
  if (t == NULL)
    assert ("_token_use: null token" == NULL);
  t->uses++;
  return t;
}
