/* Parse C expressions for cpplib.
   Copyright (C) 1987, 92, 94, 95, 97, 98, 1999, 2000 Free Software Foundation.
   Contributed by Per Bothner, 1994.

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
Boston, MA 02111-1307, USA.  */

/* Parse a C expression from text in a string  */
   
#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "hashtab.h"
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

typedef short op_t;

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
static const char * op_to_str PARAMS ((op_t, char *));

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
#define FINISHED 311

struct operation
{
  op_t op;
  U_CHAR prio;         /* Priority of op.  */
  U_CHAR flags;
  U_CHAR unsignedp;    /* True if value should be treated as unsigned.  */
  HOST_WIDEST_INT value; /* The value logically "right" of op.  */
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
      /* FIXME: assumes ASCII */
      else if (base == 16 && c >= 'a' && c <= 'f')
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
		  /* Decrement p here so that the error for an invalid
		     number will be generated below in the case where
		     this is the last character in the buffer.  */
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
      cpp_error (pfile, "too many 'l' suffixes in integer constant");
      goto error;
    }
  else if (op.unsignedp > 1)
    {
      cpp_error (pfile, "too many 'u' suffixes in integer constant");
      goto error;
    }
  
  if (base <= largest_digit)
    cpp_pedwarn (pfile,
		 "integer constant contains digits beyond the radix");

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
      if (c == '\'')
	break;
      else if (c == '\\')
	{
	  c = parse_escape (pfile, &ptr, mask);
	  if (width < HOST_BITS_PER_INT
	      && (unsigned int) c >= (unsigned int)(1 << width))
	    cpp_pedwarn (pfile,
			 "escape sequence out of range for character");
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
  enum cpp_ttype token;
  struct operation op;
  long old_written = CPP_WRITTEN (pfile);

  op.unsignedp = 0;
  op.op = INT;

  pfile->no_macro_expand++;
  token = _cpp_get_directive_token (pfile);
  if (token == CPP_OPEN_PAREN)
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
      if (_cpp_get_directive_token (pfile) != CPP_CLOSE_PAREN)
	goto oops;
    }
  CPP_SET_WRITTEN (pfile, old_written);
  pfile->no_macro_expand--;
  return op;

 oops:
  CPP_SET_WRITTEN (pfile, old_written);
  pfile->no_macro_expand--;
  cpp_error (pfile, "'defined' without an identifier");

  op.op = ERROR;
  return op;
}

struct token
{
  const char *operator;
  op_t token;
};

static const struct token tokentab2[] =
{
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
  enum cpp_ttype token;
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
      cpp_error (pfile,
		 "string constants are not allowed in #if expressions");
      op.op = ERROR;
      return op;

    case CPP_CHAR:
    case CPP_WCHAR:
      return parse_charconst (pfile, tok_start, tok_end);

    case CPP_NAME:
      if (!strncmp (tok_start, "defined", 7))
	return parse_defined (pfile);

      op.op = INT;
      op.unsignedp = 0;
      op.value = 0;

      if (CPP_OPTION (pfile, warn_undef) && !skip_evaluation)
	cpp_warning (pfile, "'%.*s' is not defined",
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
	    cpp_error (pfile, "'%s' not allowed in operand of #if",
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

/* Convert an operator ID to a string.  BUFF is a buffer at least 5
   characters long which might be used to store the string.  */
/* XXX FIXME: Remove BUFF when new lexer is implemented.  */
static const char *
op_to_str (op, buff)
     op_t op;
     char *buff;
{
  const struct token *toktab;

  /* See if it is a special token of length 2.  */
  for (toktab = tokentab2; toktab->operator != NULL; toktab++)
    if (op == toktab->token)
      return toktab->operator;

  if (ISGRAPH (op))
    sprintf (buff, "%c", (int) op);
  else
    sprintf (buff, "\\%03o", (int) op);
  return buff;
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
	cpp_pedwarn (pfile, "non-ANSI-standard escape sequence, '\\%c'", c);
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

/* Operator precedence and flags table.

After an operator is returned from the lexer, if it has priority less
than or equal to the operator on the top of the stack, we reduce the
stack by one operator and repeat the test.  Since equal priorities
reduce, this is naturally left-associative.

We handle right-associative operators by clearing the lower bit of all
left-associative operators, and setting it for right-associative ones.
After the reduction phase of a new operator, just before it is pushed
onto the stack, its RIGHT_ASSOC bit is cleared.  The effect is that
during the reduction phase, the current right-associative operator has
a priority one greater than any other operator of otherwise equal
precedence that has been pushed on the top of the stack.  This avoids
a reduction pass, and effectively makes the logic right-associative.

The remaining cases are '(' and ')'.  We handle '(' by skipping the
reduction phase completely.  ')' is given lower priority than
everything else, including '(', effectively forcing a reduction of the
parenthesised expression.  If there is no matching '(', the stack will
be reduced all the way to the beginning, exiting the parser in the
same way as the ultra-low priority end-of-expression dummy operator.
The exit code checks to see if the operator that caused it is ')', and
if so outputs an appropriate error message.

The parser assumes all shifted operators require a right operand
unless the flag NO_R_OPERAND is set, and similarly for NO_L_OPERAND.
These semantics are automatically checked, any extra semantics need to
be handled with operator-specific code.  */

#define FLAG_BITS  8
#define FLAG_MASK ((1 << FLAG_BITS) - 1)
#define PRIO_SHIFT (FLAG_BITS + 1)
#define EXTRACT_PRIO(cnst) (cnst >> FLAG_BITS)
#define EXTRACT_FLAGS(cnst) (cnst & FLAG_MASK)

/* Flags.  */
#define HAVE_VALUE     (1 << 0)
#define NO_L_OPERAND   (1 << 1)
#define NO_R_OPERAND   (1 << 2)
#define SHORT_CIRCUIT  (1 << 3)

/* Priority and flag combinations.  */
#define RIGHT_ASSOC         (1 << FLAG_BITS)
#define FORCE_REDUCE_PRIO   (0 << PRIO_SHIFT)
#define CLOSE_PAREN_PRIO    (1 << PRIO_SHIFT)
#define OPEN_PAREN_PRIO    ((2 << PRIO_SHIFT) | NO_L_OPERAND)
#define COMMA_PRIO          (3 << PRIO_SHIFT)
#define COND_PRIO          ((4 << PRIO_SHIFT) | RIGHT_ASSOC | SHORT_CIRCUIT)
#define COLON_PRIO         ((5 << PRIO_SHIFT) | SHORT_CIRCUIT)
#define OROR_PRIO          ((6 << PRIO_SHIFT) | SHORT_CIRCUIT)
#define ANDAND_PRIO        ((7 << PRIO_SHIFT) | SHORT_CIRCUIT)
#define OR_PRIO             (8 << PRIO_SHIFT)
#define XOR_PRIO            (9 << PRIO_SHIFT)
#define AND_PRIO           (10 << PRIO_SHIFT)
#define EQUAL_PRIO         (11 << PRIO_SHIFT)
#define LESS_PRIO          (12 << PRIO_SHIFT)
#define SHIFT_PRIO         (13 << PRIO_SHIFT)
#define PLUS_PRIO          (14 << PRIO_SHIFT)
#define MUL_PRIO           (15 << PRIO_SHIFT)
#define UNARY_PRIO        ((16 << PRIO_SHIFT) | RIGHT_ASSOC | NO_L_OPERAND)

#define COMPARE(OP) \
  top->unsignedp = 0; \
  top->value = (unsigned1 | unsigned2) \
  ? (unsigned HOST_WIDEST_INT) v1 OP (unsigned HOST_WIDEST_INT) v2 \
  : (v1 OP v2)
#define EQUALITY(OP) \
  top->value = v1 OP v2; \
  top->unsignedp = 0;
#define LOGICAL(OP) \
  top->value = v1 OP v2; \
  top->unsignedp = unsigned1 | unsigned2;

/* With -O2, gcc appears to produce nice code, moving the error
   message load and subsequent jump completely out of the main path.  */
#define CPP_ICE(msgid) \
  do { cpp_ice (pfile, msgid); goto syntax_error; } while(0)
#define SYNTAX_ERROR(msgid) \
  do { cpp_error (pfile, msgid); goto syntax_error; } while(0)
#define SYNTAX_ERROR2(msgid, arg) \
  do { cpp_error (pfile, msgid, arg); goto syntax_error; } while(0)

/* Parse and evaluate a C expression, reading from PFILE.
   Returns the truth value of the expression.  */

int
_cpp_parse_expr (pfile)
     cpp_reader *pfile;
{
  /* The implementation is an operator precedence parser, i.e. a
     bottom-up parser, using a stack for not-yet-reduced tokens.

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
  register struct operation *top = stack + 1;
  long old_written = CPP_WRITTEN (pfile);
  int skip_evaluation = 0;
  int result;
  char buff[5];

  pfile->parsing_if_directive++;
  /* We've finished when we try to reduce this.  */
  top->op = FINISHED;
  /* Nifty way to catch missing '('.  */
  top->prio = EXTRACT_PRIO(CLOSE_PAREN_PRIO);
  /* Avoid missing right operand checks.  */
  top->flags = NO_R_OPERAND;

  for (;;)
    {
      unsigned int prio;
      unsigned int flags;
      struct operation op;

      /* Read a token */
      op = lex (pfile, skip_evaluation);

      /* If the token is an operand, push its value and get next
	 token.  If it is an operator, get its priority and flags, and
	 try to reduce the expression on the stack.  */
      switch (op.op)
	{
	case NAME:
	  CPP_ICE ("lex returns a NAME");
	case ERROR:
	  goto syntax_error;
	case '#':
	  /* We get '#' when get_directive_token hits a syntactically
	     invalid assertion predicate.  _cpp_parse_assertion has
	     already issued an error.  */
	  goto syntax_error;
	default:
	  SYNTAX_ERROR ("invalid character in #if");

	push_immediate:
	case INT:
	case CHAR:
	  /* Push a value onto the stack.  */
	  if (top->flags & HAVE_VALUE)
	    SYNTAX_ERROR ("missing binary operator");
	  top->value = op.value;
	  top->unsignedp = op.unsignedp;
	  top->flags |= HAVE_VALUE;
	  continue;

	case '+':
	case '-':    prio = PLUS_PRIO;  if (top->flags & HAVE_VALUE) break;
          /* else unary; fall through */
	case '!':
	case '~':    prio = UNARY_PRIO;  break;

	case '*':
	case '/':
	case '%':    prio = MUL_PRIO;  break;
	case '<':
	case '>':
	case LEQ:
	case GEQ:    prio = LESS_PRIO;  break;
	case NOTEQUAL:
	case EQUAL:  prio = EQUAL_PRIO;  break;
	case LSH:
	case RSH:    prio = SHIFT_PRIO;  break;
	case '&':    prio = AND_PRIO;  break;
	case '^':    prio = XOR_PRIO;  break;
	case '|':    prio = OR_PRIO;  break;
	case ANDAND: prio = ANDAND_PRIO;  break;
	case OROR:   prio = OROR_PRIO;  break;
	case ',':    prio = COMMA_PRIO;  break;
	case '(':    prio = OPEN_PAREN_PRIO; break;
	case ')':    prio = CLOSE_PAREN_PRIO;  break;
        case ':':    prio = COLON_PRIO;  break;
        case '?':    prio = COND_PRIO;  break;
	case 0:      prio = FORCE_REDUCE_PRIO;  break;
	}

      /* Separate the operator's code into priority and flags.  */
      flags = EXTRACT_FLAGS(prio);
      prio = EXTRACT_PRIO(prio);
      if (op.op == '(')
	goto skip_reduction;

      /* Check for reductions.  Then push the operator.  */
      while (prio <= top->prio)
	{
	  HOST_WIDEST_INT v1, v2;
	  unsigned int unsigned1, unsigned2;
	  
	  /* Most operators that can appear on the stack require a
	     right operand.  Check this before trying to reduce.  */
	  if ((top->flags & (HAVE_VALUE | NO_R_OPERAND)) == 0)
	    {
	      if (top->op == '(')
		SYNTAX_ERROR ("void expression between '(' and ')'");
	      else
		SYNTAX_ERROR2 ("operator '%s' has no right operand",
			       op_to_str (top->op, buff));
	    }

	  unsigned2 = top->unsignedp, v2 = top->value;
	  top--;
	  unsigned1 = top->unsignedp, v1 = top->value;

	  /* Now set top->value = (top[1].op)(v1, v2); */
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
		  top->unsignedp = unsigned1 | unsigned2;
		  if (! top->unsignedp && ! skip_evaluation
		      && ! possible_sum_sign (v1, v2, top->value))
		    integer_overflow (pfile);
		}
	      break;
	    case '-':
	      if (!(top->flags & HAVE_VALUE))
		{ /* Unary '-' */
		  top->value = - v2;
		  if (!skip_evaluation && (top->value & v2) < 0
		      && !unsigned2)
		    integer_overflow (pfile);
		  top->unsignedp = unsigned2;
		  top->flags |= HAVE_VALUE;
		}
	      else
		{ /* Binary '-' */
		  top->value = v1 - v2;
		  top->unsignedp = unsigned1 | unsigned2;
		  if (! top->unsignedp && ! skip_evaluation
		      && ! possible_sum_sign (top->value, v2, v1))
		    integer_overflow (pfile);
		}
	      break;
	    case '*':
	      top->unsignedp = unsigned1 | unsigned2;
	      if (top->unsignedp)
		top->value = (unsigned HOST_WIDEST_INT) v1 * v2;
	      else if (!skip_evaluation)
		{
		  top->value = v1 * v2;
		  if (v1 && (top->value / v1 != v2
		             || (top->value & v1 & v2) < 0))
		    integer_overflow (pfile);
		}
	      break;
	    case '/':
	    case '%':
	      if (skip_evaluation)
		break;
	      if (v2 == 0)
		SYNTAX_ERROR ("division by zero in #if");
	      top->unsignedp = unsigned1 | unsigned2;
	      if (top[1].op == '/')
		{
		  if (top->unsignedp)
		    top->value = (unsigned HOST_WIDEST_INT) v1 / v2;
		  else
		    {
		      top->value = v1 / v2;
		      if ((top->value & v1 & v2) < 0)
			integer_overflow (pfile);
		    }
		}
	      else
		{
		  if (top->unsignedp)
		    top->value = (unsigned HOST_WIDEST_INT) v1 % v2;
		  else
		    top->value = v1 % v2;
		}
	      break;
	    case '!':
	      top->value = ! v2;
	      top->unsignedp = 0;
	      top->flags |= HAVE_VALUE;
	      break;
	    case '~':
	      top->value = ~ v2;
	      top->unsignedp = unsigned2;
	      top->flags |= HAVE_VALUE;
	      break;
	    case '<':  COMPARE(<);  break;
	    case '>':  COMPARE(>);  break;
	    case LEQ:  COMPARE(<=);  break;
	    case GEQ:  COMPARE(>=);  break;
	    case EQUAL:    EQUALITY(==);  break;
	    case NOTEQUAL: EQUALITY(!=);  break;
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
		cpp_pedwarn (pfile, "comma operator in operand of #if");
	      top->value = v2;
	      top->unsignedp = unsigned2;
	      break;
	    case '?':
	      SYNTAX_ERROR ("syntax error '?' without following ':'");
	    case ':':
	      if (top[0].op != '?')
		SYNTAX_ERROR ("syntax error ':' without preceding '?'");
	      top--;
	      if (top->value) skip_evaluation--;
	      top->value = top->value ? v1 : v2;
	      top->unsignedp = unsigned1 | unsigned2;
	      break;
	    case '(':
	      if (op.op != ')')
		SYNTAX_ERROR ("missing ')' in expression");
	      op.value = v2;
	      op.unsignedp = unsigned2;
	      goto push_immediate;
	    default:
	      SYNTAX_ERROR2 ("unimplemented operator '%s'",
			     op_to_str (top[1].op, buff));
	    case FINISHED:
	      /* Reducing this dummy operator indicates we've finished.  */
	      if (op.op == ')')
		SYNTAX_ERROR ("missing '(' in expression");
	      goto done;
	    }
	}

      /* Handle short-circuit evaluations.  */
      if (flags & SHORT_CIRCUIT)
	switch (op.op)
	  {
	  case OROR:    if (top->value) skip_evaluation++; break;
	  case ANDAND:
	  case '?':     if (!top->value) skip_evaluation++; break;
	  case ':':
	    if (top[-1].value) /* Was '?' condition true?  */
	      skip_evaluation++;
	    else
	      skip_evaluation--;
	  }

    skip_reduction:
      /* Check we have a left operand iff we need one.  */
      if (flags & NO_L_OPERAND)
	{
	  if (top->flags & HAVE_VALUE)
	    SYNTAX_ERROR2 ("missing binary operator before '%s'",
			   op_to_str (op.op, buff));
	}
      else
	{
	  if (!(top->flags & HAVE_VALUE))
	    SYNTAX_ERROR2 ("operator '%s' has no left operand",
			   op_to_str (op.op, buff));
	}

      /* Check for and handle stack overflow.  */
      top++;
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
      top->prio = prio & ~EXTRACT_PRIO(RIGHT_ASSOC);
      top->op = op.op;
    }

 done:
  result = (top[1].value != 0);
  if (top != stack)
    CPP_ICE ("unbalanced stack in #if expression");
  else if (!(top[1].flags & HAVE_VALUE))
    {
      SYNTAX_ERROR ("#if with no expression");
    syntax_error:
      _cpp_skip_rest_of_line (pfile);
      result = 0;  /* Return 0 on syntax error.  */
    }

  /* Free dynamic stack if we allocated one.  */
  if (stack != init_stack)
    free (stack);
  pfile->parsing_if_directive--;
  CPP_SET_WRITTEN (pfile, old_written);
  return result;
}
