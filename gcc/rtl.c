/* Allocate and read RTL for GNU C Compiler.
   Copyright (C) 1987, 1988, 1991, 1994 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include <ctype.h>
#include <stdio.h>
#include "rtl.h"
#include "real.h"

#include "obstack.h"
#define	obstack_chunk_alloc	xmalloc
#define	obstack_chunk_free	free

/* Obstack used for allocating RTL objects.
   Between functions, this is the permanent_obstack.
   While parsing and expanding a function, this is maybepermanent_obstack
   so we can save it if it is an inline function.
   During optimization and output, this is function_obstack.  */

extern struct obstack *rtl_obstack;

#if HOST_BITS_PER_WIDE_INT != HOST_BITS_PER_INT
extern long atol();
#endif

/* Indexed by rtx code, gives number of operands for an rtx with that code.
   Does NOT include rtx header data (code and links).
   This array is initialized in init_rtl.  */

int rtx_length[NUM_RTX_CODE + 1];

/* Indexed by rtx code, gives the name of that kind of rtx, as a C string.  */

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   NAME ,

char *rtx_name[] = {
#include "rtl.def"		/* rtl expressions are documented here */
};

#undef DEF_RTL_EXPR

/* Indexed by machine mode, gives the name of that machine mode.
   This name does not include the letters "mode".  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  NAME,

char *mode_name[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"

#ifdef EXTRA_CC_MODES
  EXTRA_CC_NAMES
#endif

};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the length of the mode, in bytes.
   GET_MODE_CLASS uses this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  CLASS,

enum mode_class mode_class[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the length of the mode, in bytes.
   GET_MODE_SIZE uses this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  SIZE,

int mode_size[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the length of the mode's subunit.
   GET_MODE_UNIT_SIZE uses this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  UNIT,

int mode_unit_size[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"		/* machine modes are documented here */
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives next wider natural mode
   (QI -> HI -> SI -> DI, etc.)  Widening multiply instructions
   use this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  \
  (enum machine_mode) WIDER,

enum machine_mode mode_wider_mode[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"		/* machine modes are documented here */
};

#undef DEF_MACHMODE

/* Indexed by mode class, gives the narrowest mode for each class.  */

enum machine_mode class_narrowest_mode[(int) MAX_MODE_CLASS];

/* Indexed by rtx code, gives a sequence of operand-types for
   rtx's of that code.  The sequence is a C string in which
   each character describes one operand.  */

char *rtx_format[] = {
  /* "*" undefined.
         can cause a warning message
     "0" field is unused (or used in a phase-dependent manner)
         prints nothing
     "i" an integer
         prints the integer
     "n" like "i", but prints entries from `note_insn_name'
     "w" an integer of width HOST_BITS_PER_WIDE_INT
         prints the integer
     "s" a pointer to a string
         prints the string
     "S" like "s", but optional:
	 the containing rtx may end before this operand
     "e" a pointer to an rtl expression
         prints the expression
     "E" a pointer to a vector that points to a number of rtl expressions
         prints a list of the rtl expressions
     "V" like "E", but optional:
	 the containing rtx may end before this operand
     "u" a pointer to another insn
         prints the uid of the insn.  */

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   FORMAT ,
#include "rtl.def"		/* rtl expressions are defined here */
#undef DEF_RTL_EXPR
};

/* Indexed by rtx code, gives a character representing the "class" of
   that rtx code.  See rtl.def for documentation on the defined classes.  */

char rtx_class[] = {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   CLASS, 
#include "rtl.def"		/* rtl expressions are defined here */
#undef DEF_RTL_EXPR
};

/* Names for kinds of NOTEs and REG_NOTEs.  */

char *note_insn_name[] = { 0                    , "NOTE_INSN_DELETED",
			   "NOTE_INSN_BLOCK_BEG", "NOTE_INSN_BLOCK_END",
			   "NOTE_INSN_LOOP_BEG", "NOTE_INSN_LOOP_END",
			   "NOTE_INSN_FUNCTION_END", "NOTE_INSN_SETJMP",
			   "NOTE_INSN_LOOP_CONT", "NOTE_INSN_LOOP_VTOP",
			   "NOTE_INSN_PROLOGUE_END", "NOTE_INSN_EPILOGUE_BEG",
			   "NOTE_INSN_DELETED_LABEL", "NOTE_INSN_FUNCTION_BEG"};

char *reg_note_name[] = { "", "REG_DEAD", "REG_INC", "REG_EQUIV", "REG_WAS_0",
			  "REG_EQUAL", "REG_RETVAL", "REG_LIBCALL",
			  "REG_NONNEG", "REG_NO_CONFLICT", "REG_UNUSED",
			  "REG_CC_SETTER", "REG_CC_USER", "REG_LABEL",
			  "REG_DEP_ANTI", "REG_DEP_OUTPUT" };

/* Allocate an rtx vector of N elements.
   Store the length, and initialize all elements to zero.  */

rtvec
rtvec_alloc (n)
     int n;
{
  rtvec rt;
  int i;

  rt = (rtvec) obstack_alloc (rtl_obstack,
			      sizeof (struct rtvec_def)
			      + (( n - 1) * sizeof (rtunion)));

  /* clear out the vector */
  PUT_NUM_ELEM(rt, n);
  for (i=0; i < n; i++)
    rt->elem[i].rtvec = NULL;	/* @@ not portable due to rtunion */

  return rt;
}

/* Allocate an rtx of code CODE.  The CODE is stored in the rtx;
   all the rest is initialized to zero.  */

rtx
rtx_alloc (code)
  RTX_CODE code;
{
  rtx rt;
  register struct obstack *ob = rtl_obstack;
  register int nelts = GET_RTX_LENGTH (code);
  register int length = sizeof (struct rtx_def)
    + (nelts - 1) * sizeof (rtunion);

  /* This function is called more than any other in GCC,
     so we manipulate the obstack directly.

     Even though rtx objects are word aligned, we may be sharing an obstack
     with tree nodes, which may have to be double-word aligned.  So align
     our length to the alignment mask in the obstack.  */

  length = (length + ob->alignment_mask) & ~ ob->alignment_mask;

  if (ob->chunk_limit - ob->next_free < length)
    _obstack_newchunk (ob, length);
  rt = (rtx)ob->object_base;
  ob->next_free += length;
  ob->object_base = ob->next_free;

  /* We want to clear everything up to the FLD array.  Normally, this is
     one int, but we don't want to assume that and it isn't very portable
     anyway; this is.  */

  length = (sizeof (struct rtx_def) - sizeof (rtunion) - 1) / sizeof (int);
  for (; length >= 0; length--)
    ((int *) rt)[length] = 0;

  PUT_CODE (rt, code);

  return rt;
}

/* Free the rtx X and all RTL allocated since X.  */

void
rtx_free (x)
     rtx x;
{
  obstack_free (rtl_obstack, x);
}

/* Create a new copy of an rtx.
   Recursively copies the operands of the rtx,
   except for those few rtx codes that are sharable.  */

rtx
copy_rtx (orig)
     register rtx orig;
{
  register rtx copy;
  register int i, j;
  register RTX_CODE code;
  register char *format_ptr;

  code = GET_CODE (orig);

  switch (code)
    {
    case REG:
    case QUEUED:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case SCRATCH:
      /* SCRATCH must be shared because they represent distinct values. */
      return orig;

    case CONST:
      /* CONST can be shared if it contains a SYMBOL_REF.  If it contains
	 a LABEL_REF, it isn't sharable.  */
      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (orig, 0), 0)) == SYMBOL_REF
	  && GET_CODE (XEXP (XEXP (orig, 0), 1)) == CONST_INT)
	return orig;
      break;

      /* A MEM with a constant address is not sharable.  The problem is that
	 the constant address may need to be reloaded.  If the mem is shared,
	 then reloading one copy of this mem will cause all copies to appear
	 to have been reloaded.  */
    }

  copy = rtx_alloc (code);
  PUT_MODE (copy, GET_MODE (orig));
  copy->in_struct = orig->in_struct;
  copy->volatil = orig->volatil;
  copy->unchanging = orig->unchanging;
  copy->integrated = orig->integrated;
  
  format_ptr = GET_RTX_FORMAT (GET_CODE (copy));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (copy)); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  XEXP (copy, i) = XEXP (orig, i);
	  if (XEXP (orig, i) != NULL)
	    XEXP (copy, i) = copy_rtx (XEXP (orig, i));
	  break;

	case '0':
	case 'u':
	  XEXP (copy, i) = XEXP (orig, i);
	  break;

	case 'E':
	case 'V':
	  XVEC (copy, i) = XVEC (orig, i);
	  if (XVEC (orig, i) != NULL)
	    {
	      XVEC (copy, i) = rtvec_alloc (XVECLEN (orig, i));
	      for (j = 0; j < XVECLEN (copy, i); j++)
		XVECEXP (copy, i, j) = copy_rtx (XVECEXP (orig, i, j));
	    }
	  break;

	case 'w':
	  XWINT (copy, i) = XWINT (orig, i);
	  break;

	case 'i':
	  XINT (copy, i) = XINT (orig, i);
	  break;

	case 's':
	case 'S':
	  XSTR (copy, i) = XSTR (orig, i);
	  break;

	default:
	  abort ();
	}
    }
  return copy;
}

/* Similar to `copy_rtx' except that if MAY_SHARE is present, it is
   placed in the result directly, rather than being copied.  */

rtx
copy_most_rtx (orig, may_share)
     register rtx orig;
     register rtx may_share;
{
  register rtx copy;
  register int i, j;
  register RTX_CODE code;
  register char *format_ptr;

  if (orig == may_share)
    return orig;

  code = GET_CODE (orig);

  switch (code)
    {
    case REG:
    case QUEUED:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
      return orig;
    }

  copy = rtx_alloc (code);
  PUT_MODE (copy, GET_MODE (orig));
  copy->in_struct = orig->in_struct;
  copy->volatil = orig->volatil;
  copy->unchanging = orig->unchanging;
  copy->integrated = orig->integrated;
  
  format_ptr = GET_RTX_FORMAT (GET_CODE (copy));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (copy)); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  XEXP (copy, i) = XEXP (orig, i);
	  if (XEXP (orig, i) != NULL && XEXP (orig, i) != may_share)
	    XEXP (copy, i) = copy_most_rtx (XEXP (orig, i), may_share);
	  break;

	case '0':
	case 'u':
	  XEXP (copy, i) = XEXP (orig, i);
	  break;

	case 'E':
	case 'V':
	  XVEC (copy, i) = XVEC (orig, i);
	  if (XVEC (orig, i) != NULL)
	    {
	      XVEC (copy, i) = rtvec_alloc (XVECLEN (orig, i));
	      for (j = 0; j < XVECLEN (copy, i); j++)
		XVECEXP (copy, i, j)
		  = copy_most_rtx (XVECEXP (orig, i, j), may_share);
	    }
	  break;

	case 'w':
	  XWINT (copy, i) = XWINT (orig, i);
	  break;

	case 'n':
	case 'i':
	  XINT (copy, i) = XINT (orig, i);
	  break;

	case 's':
	case 'S':
	  XSTR (copy, i) = XSTR (orig, i);
	  break;

	default:
	  abort ();
	}
    }
  return copy;
}

/* Subroutines of read_rtx.  */

/* Dump code after printing a message.  Used when read_rtx finds
   invalid data.  */

static void
dump_and_abort (expected_c, actual_c, infile)
     int expected_c, actual_c;
     FILE *infile;
{
  int c, i;

  if (expected_c >= 0)
    fprintf (stderr,
	     "Expected character %c.  Found character %c.",
	     expected_c, actual_c);
  fprintf (stderr, "  At file position: %ld\n", ftell (infile));
  fprintf (stderr, "Following characters are:\n\t");
  for (i = 0; i < 200; i++)
    {
      c = getc (infile);
      if (EOF == c) break;
      putc (c, stderr);
    }
  fprintf (stderr, "Aborting.\n");
  abort ();
}

/* Read chars from INFILE until a non-whitespace char
   and return that.  Comments, both Lisp style and C style,
   are treated as whitespace.
   Tools such as genflags use this function.  */

int
read_skip_spaces (infile)
     FILE *infile;
{
  register int c;
  while (c = getc (infile))
    {
      if (c == ' ' || c == '\n' || c == '\t' || c == '\f')
	;
      else if (c == ';')
	{
	  while ((c = getc (infile)) && c != '\n') ;
	}
      else if (c == '/')
	{
	  register int prevc;
	  c = getc (infile);
	  if (c != '*')
	    dump_and_abort ('*', c, infile);
	  
	  prevc = 0;
	  while (c = getc (infile))
	    {
	      if (prevc == '*' && c == '/')
		break;
	      prevc = c;
	    }
	}
      else break;
    }
  return c;
}

/* Read an rtx code name into the buffer STR[].
   It is terminated by any of the punctuation chars of rtx printed syntax.  */

static void
read_name (str, infile)
     char *str;
     FILE *infile;
{
  register char *p;
  register int c;

  c = read_skip_spaces(infile);

  p = str;
  while (1)
    {
      if (c == ' ' || c == '\n' || c == '\t' || c == '\f')
	break;
      if (c == ':' || c == ')' || c == ']' || c == '"' || c == '/'
	  || c == '(' || c == '[')
	{
	  ungetc (c, infile);
	  break;
	}
      *p++ = c;
      c = getc (infile);
    }
  if (p == str)
    {
      fprintf (stderr, "missing name or number");
      dump_and_abort (-1, -1, infile);
    }

  *p = 0;
}

/* Read an rtx in printed representation from INFILE
   and return an actual rtx in core constructed accordingly.
   read_rtx is not used in the compiler proper, but rather in
   the utilities gen*.c that construct C code from machine descriptions.  */

rtx
read_rtx (infile)
     FILE *infile;
{
  register int i, j, list_counter;
  RTX_CODE tmp_code;
  register char *format_ptr;
  /* tmp_char is a buffer used for reading decimal integers
     and names of rtx types and machine modes.
     Therefore, 256 must be enough.  */
  char tmp_char[256];
  rtx return_rtx;
  register int c;
  int tmp_int;
  HOST_WIDE_INT tmp_wide;

  /* Linked list structure for making RTXs: */
  struct rtx_list
    {
      struct rtx_list *next;
      rtx value;		/* Value of this node...		*/
    };

  c = read_skip_spaces (infile); /* Should be open paren.  */
  if (c != '(')
    dump_and_abort ('(', c, infile);

  read_name (tmp_char, infile);

  tmp_code = UNKNOWN;

  for (i=0; i < NUM_RTX_CODE; i++) /* @@ might speed this search up */
    {
      if (!(strcmp (tmp_char, GET_RTX_NAME (i))))
	{
	  tmp_code = (RTX_CODE) i;	/* get value for name */
	  break;
	}
    }
  if (tmp_code == UNKNOWN)
    {
      fprintf (stderr,
	       "Unknown rtx read in rtl.read_rtx(). Code name was %s .",
	       tmp_char);
    }
  /* (NIL) stands for an expression that isn't there.  */
  if (tmp_code == NIL)
    {
      /* Discard the closeparen.  */
      while ((c = getc (infile)) && c != ')');
      return 0;
    }

  return_rtx = rtx_alloc (tmp_code); /* if we end up with an insn expression
				       then we free this space below.  */
  format_ptr = GET_RTX_FORMAT (GET_CODE (return_rtx));

  /* If what follows is `: mode ', read it and
     store the mode in the rtx.  */

  i = read_skip_spaces (infile);
  if (i == ':')
    {
      register int k;
      read_name (tmp_char, infile);
      for (k = 0; k < NUM_MACHINE_MODES; k++)
	if (!strcmp (GET_MODE_NAME (k), tmp_char))
	  break;

      PUT_MODE (return_rtx, (enum machine_mode) k );
    }
  else
    ungetc (i, infile);

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (return_rtx)); i++)
    switch (*format_ptr++)
      {
	/* 0 means a field for internal use only.
	   Don't expect it to be present in the input.  */
      case '0':
	break;

      case 'e':
      case 'u':
	XEXP (return_rtx, i) = read_rtx (infile);
	break;

      case 'V':
	/* 'V' is an optional vector: if a closeparen follows,
	   just store NULL for this element.  */
	c = read_skip_spaces (infile);
	ungetc (c, infile);
	if (c == ')')
	  {
	    XVEC (return_rtx, i) = 0;
	    break;
 	  }
	/* Now process the vector.  */
  
      case 'E':
	{
	  register struct rtx_list *next_rtx, *rtx_list_link;
	  struct rtx_list *list_rtx;

	  c = read_skip_spaces (infile);
	  if (c != '[')
	    dump_and_abort ('[', c, infile);

	  /* add expressions to a list, while keeping a count */
	  next_rtx = NULL;
	  list_counter = 0;
	  while ((c = read_skip_spaces (infile)) && c != ']')
	    {
	      ungetc (c, infile);
	      list_counter++;
	      rtx_list_link = (struct rtx_list *)
		alloca (sizeof (struct rtx_list));
	      rtx_list_link->value = read_rtx (infile);
	      if (next_rtx == 0)
		list_rtx = rtx_list_link;
	      else
		next_rtx->next = rtx_list_link;
	      next_rtx = rtx_list_link;
	      rtx_list_link->next = 0;
	    }
	  /* get vector length and allocate it */
	  XVEC (return_rtx, i) = (list_counter
				  ? rtvec_alloc (list_counter) : NULL_RTVEC);
	  if (list_counter > 0)
	    {
	      next_rtx = list_rtx;
	      for (j = 0; j < list_counter; j++,
		   next_rtx = next_rtx->next)
		XVECEXP (return_rtx, i, j) = next_rtx->value;
	    }
	  /* close bracket gotten */
	}
	break;

      case 'S':
	/* 'S' is an optional string: if a closeparen follows,
	   just store NULL for this element.  */
	c = read_skip_spaces (infile);
	ungetc (c, infile);
	if (c == ')')
	  {
	    XSTR (return_rtx, i) = 0;
	    break;
	  }

      case 's':
	{
	  int saw_paren = 0;
	  register char *stringbuf;

	  c = read_skip_spaces (infile);
	  if (c == '(')
	    {
	      saw_paren = 1;
	      c = read_skip_spaces (infile);
	    }
	  if (c != '"')
	    dump_and_abort ('"', c, infile);

	  while (1)
	    {
	      c = getc (infile); /* Read the string  */
	      if (c == '\\')
		{
		  c = getc (infile);	/* Read the string  */
		  /* \; makes stuff for a C string constant containing
		     newline and tab.  */
		  if (c == ';')
		    {
		      obstack_grow (rtl_obstack, "\\n\\t", 4);
		      continue;
		    }
		}
	      else if (c == '"')
		break;

	      obstack_1grow (rtl_obstack, c);
	    }

	  obstack_1grow (rtl_obstack, 0);
	  stringbuf = (char *) obstack_finish (rtl_obstack);

	  if (saw_paren)
	    {
	      c = read_skip_spaces (infile);
	      if (c != ')')
		dump_and_abort (')', c, infile);
	    }
	  XSTR (return_rtx, i) = stringbuf;
	}
	break;

      case 'w':
	read_name (tmp_char, infile);
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
	tmp_wide = atoi (tmp_char);
#else
	tmp_wide = atol (tmp_char);
#endif
	XWINT (return_rtx, i) = tmp_wide;
	break;

      case 'i':
      case 'n':
	read_name (tmp_char, infile);
	tmp_int = atoi (tmp_char);
	XINT (return_rtx, i) = tmp_int;
	break;

      default:
	fprintf (stderr,
		 "switch format wrong in rtl.read_rtx(). format was: %c.\n",
		 format_ptr[-1]);
	fprintf (stderr, "\tfile position: %ld\n", ftell (infile));
	abort ();
      }

  c = read_skip_spaces (infile);
  if (c != ')')
    dump_and_abort (')', c, infile);

  return return_rtx;
}

/* This is called once per compilation, before any rtx's are constructed.
   It initializes the vector `rtx_length', the extra CC modes, if any,
   and computes certain commonly-used modes.  */

void
init_rtl ()
{
  int min_class_size[(int) MAX_MODE_CLASS];
  enum machine_mode mode;
  int i;

  for (i = 0; i < NUM_RTX_CODE; i++)
    rtx_length[i] = strlen (rtx_format[i]);

  /* Make CONST_DOUBLE bigger, if real values are bigger than
     it normally expects to have room for.
     Note that REAL_VALUE_TYPE is not defined by default,
     since tree.h is not included.  But the default dfn as `double'
     would do no harm.  */
#ifdef REAL_VALUE_TYPE
  i = sizeof (REAL_VALUE_TYPE) / sizeof (rtunion) + 2;
  if (rtx_length[(int) CONST_DOUBLE] < i)
    {
      char *s = (char *) xmalloc (i + 1);
      rtx_length[(int) CONST_DOUBLE] = i;
      rtx_format[(int) CONST_DOUBLE] = s;
      *s++ = 'e';
      *s++ = '0';
      /* Set the GET_RTX_FORMAT of CONST_DOUBLE to a string
	 of as many `w's as we now have elements.  Subtract two from
	 the size to account for the 'e' and the '0'.  */
      for (i = 2; i < rtx_length[(int) CONST_DOUBLE]; i++)
	*s++ = 'w';
      *s++ = 0;
    }
#endif

#ifdef EXTRA_CC_MODES
  for (i = (int) CCmode + 1; i < (int) MAX_MACHINE_MODE; i++)
    {
      mode_class[i] = MODE_CC;
      mode_size[i] = mode_size[(int) CCmode];
      mode_unit_size[i] = mode_unit_size[(int) CCmode];
      mode_wider_mode[i - 1] = (enum machine_mode) i;
      mode_wider_mode[i] = VOIDmode;
    }
#endif

  /* Find the narrowest mode for each class.  */

  for (i = 0; i < (int) MAX_MODE_CLASS; i++)
    min_class_size[i] = 1000;

  for (mode = VOIDmode; (int) mode < (int) MAX_MACHINE_MODE;
       mode = (enum machine_mode) ((int) mode + 1))
    {
      if (GET_MODE_SIZE (mode) < min_class_size[(int) GET_MODE_CLASS (mode)])
	{
	  class_narrowest_mode[(int) GET_MODE_CLASS (mode)] = mode;
	  min_class_size[(int) GET_MODE_CLASS (mode)] = GET_MODE_SIZE (mode);
	}
    }
}

#ifdef memset
gcc_memset (dest, value, len)
     char *dest;
     int value;
     int len;
{
  while (len-- > 0)
    *dest++ = value;
}
#endif /* memset */
