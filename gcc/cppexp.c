/* Parse C expressions for cpplib.
   Copyright (C) 1987, 92, 94, 95, 97, 98, 1999, 2000 Free Software Foundation.

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
Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!

Written by Per Bothner 1994.  */

/* Parse a C expression from text in a string  */
   
#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "cpphash.h"

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE INT_TYPE_SIZE
#endif

#ifndef MAX_CHAR_TYPE_SIZE
#define MAX_CHAR_TYPE_SIZE CHAR_TYPE_SIZE
#endif

#ifndef MAX_INT_TYPE_SIZE
#define MAX_INT_TYPE_SIZE INT_TYPE_SIZE
#endif

#ifndef MAX_LONG_TYPE_SIZE
#define MAX_LONG_TYPE_SIZE LONG_TYPE_SIZE
#endif

#ifndef MAX_WCHAR_TYPE_SIZE
#define MAX_WCHAR_TYPE_SIZE WCHAR_TYPE_SIZE
#endif

#define MAX_CHAR_TYPE_MASK (MAX_CHAR_TYPE_SIZE < HOST_BITS_PER_WIDEST_INT \
			    ? (~(~(HOST_WIDEST_INT) 0 << MAX_CHAR_TYPE_SIZE)) \
			    : ~ (HOST_WIDEST_INT) 0)

#define MAX_WCHAR_TYPE_MASK (MAX_WCHAR_TYPE_SIZE < HOST_BITS_PER_WIDEST_INT \
			     ? ~(~(HOST_WIDEST_INT) 0 << MAX_WCHAR_TYPE_SIZE) \
			     : ~ (HOST_WIDEST_INT) 0)

/* Yield nonzero if adding two numbers with A's and B's signs can yield a
   number with SUM's sign, where A, B, and SUM are all C integers.  */
#define possible_sum_sign(a, b, sum) ((((a) ^ (b)) | ~ ((a) ^ (sum))) < 0)

static void integer_overflow PARAMS ((cpp_reader *));
static HOST_WIDEST_INT left_shift PARAMS ((cpp_reader *, HOST_WIDEST_INT,
					   unsigned int,
					   unsigned HOST_WIDEST_INT));
static HOST_WIDEST_INT right_shift PARAMS ((cpp_reader *, HOST_WIDEST_INT,
					    unsigned int,
					    unsigned HOST_WIDEST_INT));
static struct operation parse_number PARAMS ((cpp_reader *, U_CHAR *,
					      U_CHAR *));
static struct operation parse_charconst PARAMS ((cpp_reader *, U_CHAR *,
						 U_CHAR *));
static struct operation parse_defined PARAMS ((cpp_reader *));
static HOST_WIDEST_INT parse_escape PARAMS ((cpp_reader *, U_CHAR **,
					     HOST_WIDEST_INT));
static struct operation lex PARAMS ((cpp_reader *, int));


#define ERROR 299
#define OROR 300
#define ANDAND 301
#define EQUAL 302
#define NOTEQUAL 303
#define LEQ 304
#define GEQ 305
#define LSH 306
#define RSH 307
#define NAME 308
#define INT 309
#define CHAR 310

struct operation
{
  short op;
  U_CHAR rprio; /* Priority of op (relative to it right operand).  */
  U_CHAR flags;
  U_CHAR unsignedp;    /* true if value should be treated as unsigned */
  HOST_WIDEST_INT value;        /* The value logically "right" of op.  */
};

/* Parse and convert an integer for #if.  Accepts decimal, hex, or octal
   with or without size suffixes.  */

static struct operation
parse_number (pfile, start, end)
     cpp_reader *pfile;
     U_CHAR *start;
     U_CHAR *end;
{
  struct operation op;
  U_CHAR *p = start;
  int c;
  unsigned HOST_WIDEST_INT n = 0, nd, MAX_over_base;
  int base = 10;
  int overflow = 0;
  int digit, largest_digit = 0;
  int spec_long = 0;

  op.unsignedp = 0;

  if (p[0] == '0')
    {
      if (end - start >= 3 && (p[1] == 'x' || p[1] == 'X'))
	{
	  p += 2;
	  base = 16;
	}
      else
	{
	  p += 1;
	  base = 8;
	}
    }

  /* Some buggy compilers (e.g. MPW C) seem to need both casts.  */
  MAX_over_base = (((unsigned HOST_WIDEST_INT) -1)
		   / ((unsigned HOST_WIDEST_INT) base));

  while (p < end)
    {
      c = *p++;

      if (c >= '0' && c <= '9')
	digit = c - '0';
      else if (base == 16 && c >= 'a' && c <= 'f') /* FIXME: assumes ASCII */
	digit = c - 'a' + 10;
      else if (base == 16 && c >= 'A' && c <= 'F')
	digit = c - 'A' + 10;
      else if (c == '.')
	{
	  /* It's a float since it contains a point.  */
	  cpp_error (pfile,
		"floating point numbers are not allowed in #if expressions");
	  goto error;
	}
      else
	{
	  /* `l' means long, and `u' means unsigned.  */
	  for (;;)
	    {
	      if (c == 'l' || c == 'L')
		  spec_long++;
	      else if (c == 'u' || c == 'U')
		  op.unsignedp++;
	      else
		{
		  /* Decrement p here so that the error for an invalid number
		     will be generated below in the case where this is the
		     last character in the buffer.  */
		  p--;
		  break;
		}
	      if (p == end)
		break;
	      c = *p++;
	    }
	  /* Don't look for any more digits after the suffixes.  */
	  break;
	}
      
      if (largest_digit < digit)
	largest_digit = digit;
      nd = n * base + digit;
      overflow |= MAX_over_base < n || nd < n;
      n = nd;
    }

  if (p != end)
    {
      cpp_error (pfile, "invalid number in #if expression");
      goto error;
    }
  else if (spec_long > (CPP_OPTION (pfile, c89) ? 1 : 2))
    {
      cpp_error (pfile, "too many `l' suffixes in integer constant");
      goto error;
    }
  else if (op.unsignedp > 1)
    {
      cpp_error (pfile, "too many `u' suffixes in integer constant");
      goto error;
    }
  
  if (base <= largest_digit)
    cpp_pedwarn (pfile, "integer constant contains digits beyond the radix");

  if (overflow)
    cpp_pedwarn (pfile, "integer constant out of range");

  /* If too big to be signed, consider it unsigned.  */
  else if ((HOST_WIDEST_INT) n < 0 && ! op.unsignedp)
    {
      if (base == 10)
	cpp_warning (pfile,
		     "integer constant is so large that it is unsigned");
      op.unsignedp = 1;
    }

  op.value = n;
  op.op = INT;
  return op;

 error:
  op.op = ERROR;
  return op;
}

/* Parse and convert a character constant for #if.  Understands backslash
   escapes (\n, \031) and multibyte characters (if so configured).  */
static struct operation
parse_charconst (pfile, start, end)
     cpp_reader *pfile;
     U_CHAR *start;
     U_CHAR *end;
{
  struct operation op;
  HOST_WIDEST_INT result = 0;
  int num_chars = 0;
  int num_bits;
  unsigned int width = MAX_CHAR_TYPE_SIZE, mask = MAX_CHAR_TYPE_MASK;
  int max_chars;
  U_CHAR *ptr = start;

  int c = -1;

  if (*ptr == 'L')
    {
      ++ptr;
      width = MAX_WCHAR_TYPE_SIZE, mask = MAX_WCHAR_TYPE_MASK;
    }
  max_chars = MAX_LONG_TYPE_SIZE / width;

  ++ptr;  /* skip initial quote */

  while (ptr < end)
    {
      c = *ptr++;
      if (c == '\'' || c == '\0')
	break;
      else if (c == '\\')
	{
	  c = parse_escape (pfile, &ptr, mask);
	  if (width < HOST_BITS_PER_INT
	      && (unsigned int) c >= (unsigned int)(1 << width))
	    cpp_pedwarn (pfile, "escape sequence out of range for character");
	}
	  
      /* Merge character into result; ignore excess chars.  */
      if (++num_chars <= max_chars)
	{
	  if (width < HOST_BITS_PER_INT)
	    result = (result << width) | (c & ((1 << width) - 1));
	  else
	    result = c;
	}
    }

  if (num_chars == 0)
    {
      cpp_error (pfile, "empty character constant");
      goto error;
    }
  else if (c != '\'')
    {
      /* cpp_get_token has already emitted an error if !traditional. */
      if (! CPP_TRADITIONAL (pfile))
	cpp_error (pfile, "malformatted character constant");
      goto error;
    }
  else if (num_chars > max_chars)
    {
      cpp_error (pfile, "character constant too long");
      goto error;
    }
  else if (num_chars != 1 && ! CPP_TRADITIONAL (pfile))
    cpp_warning (pfile, "multi-character character constant");

  /* If char type is signed, sign-extend the constant.  */
  num_bits = num_chars * width;
      
  if (cpp_defined (pfile, (const U_CHAR *)"__CHAR_UNSIGNED__",
		   sizeof ("__CHAR_UNSIGNED__")-1)
      || ((result >> (num_bits - 1)) & 1) == 0)
    op.value = result & ((unsigned HOST_WIDEST_INT) ~0
			 >> (HOST_BITS_PER_WIDEST_INT - num_bits));
  else
    op.value = result | ~((unsigned HOST_WIDEST_INT) ~0
			  >> (HOST_BITS_PER_WIDEST_INT - num_bits));

  /* This is always a signed type.  */
  op.unsignedp = 0;
  op.op = CHAR;
  return op;

 error:
  op.op = ERROR;
  return op;
}

static struct operation
parse_defined (pfile)
     cpp_reader *pfile;
{
  int paren = 0, len;
  U_CHAR *tok;
  enum cpp_token token;
  struct operation op;
  long old_written = CPP_WRITTEN (pfile);

  op.unsignedp = 0;
  op.op = INT;

  pfile->no_macro_expand++;
  token = _cpp_get_directive_token (pfile);
  if (token == CPP_LPAREN)
    {
      paren++;
      CPP_SET_WRITTEN (pfile, old_written);
      token = _cpp_get_directive_token (pfile);
    }

  if (token != CPP_NAME)
    goto oops;

  tok = pfile->token_buffer + old_written;
  len = CPP_PWRITTEN (pfile) - tok;
  op.value = cpp_defined (pfile, tok, len);

  if (paren)
    {
      if (_cpp_get_directive_token (pfile) != CPP_RPAREN)
	goto oops;
    }
  CPP_SET_WRITTEN (pfile, old_written);
  pfile->no_macro_expand--;
  return op;

 oops:
  CPP_SET_WRITTEN (pfile, old_written);
  pfile->no_macro_expand--;
  cpp_error (pfile, "`defined' without an identifier");

  op.op = ERROR;
  return op;
}


struct token {
  const char *operator;
  int token;
};

static const struct token tokentab2[] = {
  {"&&", ANDAND},
  {"||", OROR},
  {"<<", LSH},
  {">>", RSH},
  {"==", EQUAL},
  {"!=", NOTEQUAL},
  {"<=", LEQ},
  {">=", GEQ},
  {"++", ERROR},
  {"--", ERROR},
  {NULL, ERROR}
};

/* Read one token.  */

static struct operation
lex (pfile, skip_evaluation)
     cpp_reader *pfile;
     int skip_evaluation;
{
  const struct token *toktab;
  enum cpp_token token;
  struct operation op;
  U_CHAR *tok_start, *tok_end;
  long old_written;

  old_written = CPP_WRITTEN (pfile);
  token = _cpp_get_directive_token (pfile);

  tok_start = pfile->token_buffer + old_written;
  tok_end = CPP_PWRITTEN (pfile);
  CPP_SET_WRITTEN (pfile, old_written);
  switch (token)
    {
    case CPP_EOF: /* Should not happen ...  */
    case CPP_VSPACE:
      op.op = 0;
      return op;
    case CPP_NUMBER:
      return parse_number (pfile, tok_start, tok_end);
    case CPP_STRING:
    case CPP_WSTRING:
      cpp_error (pfile, "string constants are not allowed in #if expressions");
      op.op = ERROR;
      return op;

    case CPP_CHAR:
    case CPP_WCHAR:
      return parse_charconst (pfile, tok_start, tok_end);

    case CPP_NAME:
      if (!strcmp (tok_start, "defined"))
	return parse_defined (pfile);

      op.op = INT;
      op.unsignedp = 0;
      op.value = 0;

      if (CPP_OPTION (pfile, warn_undef) && !skip_evaluation)
	cpp_warning (pfile, "`%.*s' is not defined",
		     (int) (tok_end - tok_start), tok_start);
      return op;

    case CPP_ASSERTION:
      op.op = INT;
      op.unsignedp = 0;
      op.value = cpp_defined (pfile, tok_start, tok_end - tok_start);
      return op;

    case CPP_OTHER:
      /* See if it is a special token of length 2.  */
      if (tok_start + 2 == tok_end)
        {
	  for (toktab = tokentab2; toktab->operator != NULL; toktab++)
	    if (tok_start[0] == toktab->operator[0]
		&& tok_start[1] == toktab->operator[1])
		break;
	  if (toktab->token == ERROR)
	    cpp_error (pfile, "`%s' not allowed in operand of `#if'",
		       tok_start);
	  op.op = toktab->token; 
	  return op;
	}
      /* fall through */
    default:
      op.op = *tok_start;
      return op;
  }
}


/* Parse a C escape sequence.  STRING_PTR points to a variable
   containing a pointer to the string to parse.  That pointer
   is updated past the characters we use.  The value of the
   escape sequence is returned.

   A negative value means the sequence \ newline was seen,
   which is supposed to be equivalent to nothing at all.

   If \ is followed by a null character, we return a negative
   value and leave the string pointer pointing at the null character.

   If \ is followed by 000, we return 0 and leave the string pointer
   after the zeros.  A value of 0 does not mean end of string.  */

static HOST_WIDEST_INT
parse_escape (pfile, string_ptr, result_mask)
     cpp_reader *pfile;
     U_CHAR **string_ptr;
     HOST_WIDEST_INT result_mask;
{
  register int c = *(*string_ptr)++;
  switch (c)
    {
    case 'a':
      return TARGET_BELL;
    case 'b':
      return TARGET_BS;
    case 'e':
    case 'E':
      if (CPP_PEDANTIC (pfile))
	cpp_pedwarn (pfile, "non-ANSI-standard escape sequence, `\\%c'", c);
      return TARGET_ESC;
    case 'f':
      return TARGET_FF;
    case 'n':
      return TARGET_NEWLINE;
    case 'r':
      return TARGET_CR;
    case 't':
      return TARGET_TAB;
    case 'v':
      return TARGET_VT;
    case '\n':
      return -2;
    case 0:
      (*string_ptr)--;
      return 0;
      
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      {
	register HOST_WIDEST_INT i = c - '0';
	register int count = 0;
	while (++count < 3)
	  {
	    c = *(*string_ptr)++;
	    if (c >= '0' && c <= '7')
	      i = (i << 3) + c - '0';
	    else
	      {
		(*string_ptr)--;
		break;
	      }
	  }
	if (i != (i & result_mask))
	  {
	    i &= result_mask;
	    cpp_pedwarn (pfile, "octal escape sequence out of range");
	  }
	return i;
      }
    case 'x':
      {
	register unsigned HOST_WIDEST_INT i = 0, overflow = 0;
	register int digits_found = 0, digit;
	for (;;)
	  {
	    c = *(*string_ptr)++;
	    if (c >= '0' && c <= '9')
	      digit = c - '0';
	    else if (c >= 'a' && c <= 'f')
	      digit = c - 'a' + 10;
	    else if (c >= 'A' && c <= 'F')
	      digit = c - 'A' + 10;
	    else
	      {
		(*string_ptr)--;
		break;
	      }
	    overflow |= i ^ (i << 4 >> 4);
	    i = (i << 4) + digit;
	    digits_found = 1;
	  }
	if (!digits_found)
	  cpp_error (pfile, "\\x used with no following hex digits");
	if (overflow | (i != (i & result_mask)))
	  {
	    i &= result_mask;
	    cpp_pedwarn (pfile, "hex escape sequence out of range");
	  }
	return i;
      }
    default:
      return c;
    }
}

static void
integer_overflow (pfile)
     cpp_reader *pfile;
{
  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "integer overflow in preprocessor expression");
}

static HOST_WIDEST_INT
left_shift (pfile, a, unsignedp, b)
     cpp_reader *pfile;
     HOST_WIDEST_INT a;
     unsigned int unsignedp;
     unsigned HOST_WIDEST_INT b;
{
  if (b >= HOST_BITS_PER_WIDEST_INT)
    {
      if (! unsignedp && a != 0)
	integer_overflow (pfile);
      return 0;
    }
  else if (unsignedp)
    return (unsigned HOST_WIDEST_INT) a << b;
  else
    {
      HOST_WIDEST_INT l = a << b;
      if (l >> b != a)
	integer_overflow (pfile);
      return l;
    }
}

static HOST_WIDEST_INT
right_shift (pfile, a, unsignedp, b)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     HOST_WIDEST_INT a;
     unsigned int unsignedp;
     unsigned HOST_WIDEST_INT b;
{
  if (b >= HOST_BITS_PER_WIDEST_INT)
    return unsignedp ? 0 : a >> (HOST_BITS_PER_WIDEST_INT - 1);
  else if (unsignedp)
    return (unsigned HOST_WIDEST_INT) a >> b;
  else
    return a >> b;
}

/* These priorities are all even, so we can handle associatively.  */
#define PAREN_INNER_PRIO 2
#define COMMA_PRIO 4
#define COND_PRIO (COMMA_PRIO+2)
#define OROR_PRIO (COND_PRIO+2)
#define ANDAND_PRIO (OROR_PRIO+2)
#define OR_PRIO (ANDAND_PRIO+2)
#define XOR_PRIO (OR_PRIO+2)
#define AND_PRIO (XOR_PRIO+2)
#define EQUAL_PRIO (AND_PRIO+2)
#define LESS_PRIO (EQUAL_PRIO+2)
#define SHIFT_PRIO (LESS_PRIO+2)
#define PLUS_PRIO (SHIFT_PRIO+2)
#define MUL_PRIO (PLUS_PRIO+2)
#define UNARY_PRIO (MUL_PRIO+2)
#define PAREN_OUTER_PRIO (UNARY_PRIO+2)

#define LEFT_OPERAND_REQUIRED 1
#define RIGHT_OPERAND_REQUIRED 2
#define HAVE_VALUE 4
#define SIGN_QUALIFIED 8

#define COMPARE(OP) \
  top->unsignedp = 0;\
  top->value = (unsigned1 || unsigned2) \
  ? (unsigned HOST_WIDEST_INT) v1 OP (unsigned HOST_WIDEST_INT) v2 : (v1 OP v2)

/* Parse and evaluate a C expression, reading from PFILE.
   Returns the truth value of the expression.  */

int
_cpp_parse_expr (pfile)
     cpp_reader *pfile;
{
  /* The implementation is an operator precedence parser,
     i.e. a bottom-up parser, using a stack for not-yet-reduced tokens.

     The stack base is 'stack', and the current stack pointer is 'top'.
     There is a stack element for each operator (only),
     and the most recently pushed operator is 'top->op'.
     An operand (value) is stored in the 'value' field of the stack
     element of the operator that precedes it.
     In that case the 'flags' field has the HAVE_VALUE flag set.  */

#define INIT_STACK_SIZE 20
  struct operation init_stack[INIT_STACK_SIZE];
  struct operation *stack = init_stack;
  struct operation *limit = stack + INIT_STACK_SIZE;
  register struct operation *top = stack;
  unsigned int lprio, rprio = 0;
  int skip_evaluation = 0;
  long old_written = CPP_WRITTEN (pfile);
  int result;

  pfile->parsing_if_directive++;
  top->rprio = 0;
  top->flags = 0;
  for (;;)
    {
      struct operation op;
      U_CHAR flags = 0;

      /* Read a token */
      op =  lex (pfile, skip_evaluation);

      /* See if the token is an operand, in which case go to set_value.
	 If the token is an operator, figure out its left and right
	 priorities, and then goto maybe_reduce.  */

      switch (op.op)
	{
	case NAME:
	  cpp_ice (pfile, "lex returns a NAME");
	  goto syntax_error;
	case INT:  case CHAR:
	  goto set_value;
	case 0:
	  lprio = 0;  goto maybe_reduce;
	case '+':  case '-':
	  if (top->flags & HAVE_VALUE)
	    {
	      lprio = PLUS_PRIO;
	      goto binop;
	    }
	  if (top->flags & SIGN_QUALIFIED)
	    {
	      cpp_error (pfile, "more than one sign operator given");
	      goto syntax_error;
	    }
	  flags = SIGN_QUALIFIED;
	  /* else fall through */
	case '!':  case '~':
	  flags |= RIGHT_OPERAND_REQUIRED;
	  rprio = UNARY_PRIO;  lprio = rprio + 1;  goto maybe_reduce;
	case '*':  case '/':  case '%':
	  lprio = MUL_PRIO;  goto binop;
	case '<':  case '>':  case LEQ:  case GEQ:
	  lprio = LESS_PRIO;  goto binop;
	case EQUAL:  case NOTEQUAL:
	  lprio = EQUAL_PRIO;  goto binop;
	case LSH:  case RSH:
	  lprio = SHIFT_PRIO;  goto binop;
	case '&':  lprio = AND_PRIO;  goto binop;
	case '^':  lprio = XOR_PRIO;  goto binop;
	case '|':  lprio = OR_PRIO;  goto binop;
	case ANDAND:  lprio = ANDAND_PRIO;  goto binop;
	case OROR:  lprio = OROR_PRIO;  goto binop;
	case ',':
	  lprio = COMMA_PRIO;  goto binop;
	case '(':
	  lprio = PAREN_OUTER_PRIO;  rprio = PAREN_INNER_PRIO;
	  goto maybe_reduce;
	case ')':
	  lprio = PAREN_INNER_PRIO;  rprio = PAREN_OUTER_PRIO;
	  flags = HAVE_VALUE;	/* At least, we will have after reduction.  */
	  goto maybe_reduce;
        case ':':
	  lprio = COND_PRIO;  rprio = COND_PRIO + 1;
	  goto maybe_reduce;
        case '?':
	  lprio = COND_PRIO;  rprio = COND_PRIO;
	  goto maybe_reduce;
	case ERROR:
	  goto syntax_error;
	default:
	  cpp_error (pfile, "invalid character in #if");
	  goto syntax_error;
	}

    set_value:
      /* Push a value onto the stack.  */
      if (top->flags & HAVE_VALUE)
	{
	  cpp_error (pfile, "syntax error in #if");
	  goto syntax_error;
	}
      top->value = op.value;
      top->unsignedp = op.unsignedp;
      top->flags |= HAVE_VALUE;
      continue;

    binop:
      flags = LEFT_OPERAND_REQUIRED|RIGHT_OPERAND_REQUIRED;
      rprio = lprio + 1;

    maybe_reduce:
      /* Push an operator, and check if we can reduce now.  */
      while (top->rprio > lprio)
	{
	  HOST_WIDEST_INT v1 = top[-1].value, v2 = top[0].value;
	  unsigned int unsigned1 = top[-1].unsignedp;
	  unsigned int unsigned2 = top[0].unsignedp;
	  top--;
	  if ((top[1].flags & LEFT_OPERAND_REQUIRED)
	      && ! (top[0].flags & HAVE_VALUE))
	    {
	      cpp_error (pfile, "syntax error - missing left operand");
	      goto syntax_error;
	    }
	  if ((top[1].flags & RIGHT_OPERAND_REQUIRED)
	      && ! (top[1].flags & HAVE_VALUE))
	    {
	      cpp_error (pfile, "syntax error - missing right operand");
	      goto syntax_error;
	    }
	  /* top[0].value = (top[1].op)(v1, v2);*/
	  switch (top[1].op)
	    {
	    case '+':
	      if (!(top->flags & HAVE_VALUE))
		{ /* Unary '+' */
		  top->value = v2;
		  top->unsignedp = unsigned2;
		  top->flags |= HAVE_VALUE;
		}
	      else
		{
		  top->value = v1 + v2;
		  top->unsignedp = unsigned1 || unsigned2;
		  if (! top->unsignedp && ! skip_evaluation
		      && ! possible_sum_sign (v1, v2, top->value))
		    integer_overflow (pfile);
		}
	      break;
	    case '-':
	      if (!(top->flags & HAVE_VALUE))
		{ /* Unary '-' */
		  top->value = - v2;
		  if (!skip_evaluation && (top->value & v2) < 0 && !unsigned2)
		    integer_overflow (pfile);
		  top->unsignedp = unsigned2;
		  top->flags |= HAVE_VALUE;
		}
	      else
		{ /* Binary '-' */
		  top->value = v1 - v2;
		  top->unsignedp = unsigned1 || unsigned2;
		  if (! top->unsignedp && ! skip_evaluation
		      && ! possible_sum_sign (top->value, v2, v1))
		    integer_overflow (pfile);
		}
	      break;
	    case '*':
	      top->unsignedp = unsigned1 || unsigned2;
	      if (top->unsignedp)
		top->value = (unsigned HOST_WIDEST_INT) v1 * v2;
	      else if (!skip_evaluation)
		{
		  top->value = v1 * v2;
		  if (v1
		      && (top->value / v1 != v2
			  || (top->value & v1 & v2) < 0))
		    integer_overflow (pfile);
		}
	      break;
	    case '/':
	      if (skip_evaluation)
		break;
	      if (v2 == 0)
		{
		  cpp_error (pfile, "division by zero in #if");
		  v2 = 1;
		}
	      top->unsignedp = unsigned1 || unsigned2;
	      if (top->unsignedp)
		top->value = (unsigned HOST_WIDEST_INT) v1 / v2;
	      else
		{
		  top->value = v1 / v2;
		  if ((top->value & v1 & v2) < 0)
		    integer_overflow (pfile);
		}
	      break;
	    case '%':
	      if (skip_evaluation)
		break;
	      if (v2 == 0)
		{
		  cpp_error (pfile, "division by zero in #if");
		  v2 = 1;
		}
	      top->unsignedp = unsigned1 || unsigned2;
	      if (top->unsignedp)
		top->value = (unsigned HOST_WIDEST_INT) v1 % v2;
	      else
		top->value = v1 % v2;
	      break;
	    case '!':
	      if (top->flags & HAVE_VALUE)
		{
		  cpp_error (pfile, "syntax error");
		  goto syntax_error;
		}
	      top->value = ! v2;
	      top->unsignedp = 0;
	      top->flags |= HAVE_VALUE;
	      break;
	    case '~':
	      if (top->flags & HAVE_VALUE)
		{
		  cpp_error (pfile, "syntax error");
		  goto syntax_error;
		}
	      top->value = ~ v2;
	      top->unsignedp = unsigned2;
	      top->flags |= HAVE_VALUE;
	      break;
	    case '<':  COMPARE(<);  break;
	    case '>':  COMPARE(>);  break;
	    case LEQ:  COMPARE(<=); break;
	    case GEQ:  COMPARE(>=); break;
	    case EQUAL:
	      top->value = (v1 == v2);
	      top->unsignedp = 0;
	      break;
	    case NOTEQUAL:
	      top->value = (v1 != v2);
	      top->unsignedp = 0;
	      break;
	    case LSH:
	      if (skip_evaluation)
		break;
	      top->unsignedp = unsigned1;
	      if (v2 < 0 && ! unsigned2)
		top->value = right_shift (pfile, v1, unsigned1, -v2);
	      else
		top->value = left_shift (pfile, v1, unsigned1, v2);
	      break;
	    case RSH:
	      if (skip_evaluation)
		break;
	      top->unsignedp = unsigned1;
	      if (v2 < 0 && ! unsigned2)
		top->value = left_shift (pfile, v1, unsigned1, -v2);
	      else
		top->value = right_shift (pfile, v1, unsigned1, v2);
	      break;
#define LOGICAL(OP) \
	      top->value = v1 OP v2;\
	      top->unsignedp = unsigned1 || unsigned2;
	    case '&':  LOGICAL(&); break;
	    case '^':  LOGICAL(^);  break;
	    case '|':  LOGICAL(|);  break;
	    case ANDAND:
	      top->value = v1 && v2;  top->unsignedp = 0;
	      if (!v1) skip_evaluation--;
	      break;
	    case OROR:
	      top->value = v1 || v2;  top->unsignedp = 0;
	      if (v1) skip_evaluation--;
	      break;
	    case ',':
	      if (CPP_PEDANTIC (pfile))
		cpp_pedwarn (pfile, "comma operator in operand of `#if'");
	      top->value = v2;
	      top->unsignedp = unsigned2;
	      break;
	    case '(':  case '?':
	      cpp_error (pfile, "syntax error in #if");
	      goto syntax_error;
	    case ':':
	      if (top[0].op != '?')
		{
		  cpp_error (pfile,
			     "syntax error ':' without preceding '?'");
		  goto syntax_error;
		}
	      else if (! (top[1].flags & HAVE_VALUE)
		       || !(top[-1].flags & HAVE_VALUE)
		       || !(top[0].flags & HAVE_VALUE))
		{
		  cpp_error (pfile, "bad syntax for ?: operator");
		  goto syntax_error;
		}
	      else
		{
		  top--;
		  if (top->value) skip_evaluation--;
		  top->value = top->value ? v1 : v2;
		  top->unsignedp = unsigned1 || unsigned2;
		}
	      break;
	    case ')':
	      if (! (top[0].flags & HAVE_VALUE)
		  || top[0].op != '('
		  || (top[-1].flags & HAVE_VALUE))
		{
		  cpp_error (pfile, "mismatched parentheses in #if");
		  goto syntax_error;
		}
	      else
		{
		  top--;
		  top->value = v1;
		  top->unsignedp = unsigned1;
		  top->flags |= HAVE_VALUE;
		}
	      break;
	    default:
	      if (ISGRAPH (top[1].op))
		cpp_error (pfile, "unimplemented operator '%c'\n", top[1].op);
	      else
		cpp_error (pfile, "unimplemented operator '\\%03o'\n",
			   top[1].op);
	    }
	}
      if (op.op == 0)
	{
	  if (top != stack)
	    cpp_ice (pfile, "unbalanced stack in #if expression");
	  if (!(top->flags & HAVE_VALUE))
	    cpp_error (pfile, "#if with no expression");
	  result = (top->value != 0);
	  goto done;
	}
      top++;
      
      /* Check for and handle stack overflow.  */
      if (top == limit)
	{
	  struct operation *new_stack;
	  int old_size = (char *) limit - (char *) stack;
	  int new_size = 2 * old_size;
	  if (stack != init_stack)
	    new_stack = (struct operation *) xrealloc (stack, new_size);
	  else
	    {
	      new_stack = (struct operation *) xmalloc (new_size);
	      memcpy (new_stack, stack, old_size);
	    }
	  stack = new_stack;
	  top = (struct operation *) ((char *) new_stack + old_size);
	  limit = (struct operation *) ((char *) new_stack + new_size);
	}
      
      top->flags = flags;
      top->rprio = rprio;
      top->op = op.op;
      if ((op.op == OROR && top[-1].value)
	  || (op.op == ANDAND && !top[-1].value)
	  || (op.op == '?' && !top[-1].value))
	{
	  skip_evaluation++;
	}
      else if (op.op == ':')
	{
	  if (top[-2].value) /* Was condition true? */
	    skip_evaluation++;
	  else
	    skip_evaluation--;
	}
    }
 syntax_error:
  _cpp_skip_rest_of_line (pfile);
  result = 0;
 done:
  pfile->parsing_if_directive--;
  CPP_SET_WRITTEN (pfile, old_written);
  if (stack != init_stack)
    free (stack);
  return result;
}
