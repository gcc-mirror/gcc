/* Parse C expressions for cpplib.
   Copyright (C) 1987, 1992, 1994, 1995, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation.
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

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "cpphash.h"

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
static struct op parse_number PARAMS ((cpp_reader *, const cpp_token *));
static struct op parse_defined PARAMS ((cpp_reader *));
static struct op lex PARAMS ((cpp_reader *, int, cpp_token *));
static const unsigned char *op_as_text PARAMS ((cpp_reader *, enum cpp_ttype));

struct op
{
  enum cpp_ttype op;
  U_CHAR prio;         /* Priority of op.  */
  U_CHAR flags;
  U_CHAR unsignedp;    /* True if value should be treated as unsigned.  */
  HOST_WIDEST_INT value; /* The value logically "right" of op.  */
};

/* There is no "error" token, but we can't get comments in #if, so we can
   abuse that token type.  */
#define CPP_ERROR CPP_COMMENT

/* With -O2, gcc appears to produce nice code, moving the error
   message load and subsequent jump completely out of the main path.  */
#define CPP_ICE(msgid) \
  do { cpp_ice (pfile, msgid); goto syntax_error; } while(0)
#define SYNTAX_ERROR(msgid) \
  do { cpp_error (pfile, msgid); goto syntax_error; } while(0)
#define SYNTAX_ERROR2(msgid, arg) \
  do { cpp_error (pfile, msgid, arg); goto syntax_error; } while(0)

/* Parse and convert an integer for #if.  Accepts decimal, hex, or octal
   with or without size suffixes.  */
struct suffix
{
  unsigned char s[4];
  unsigned char u;
  unsigned char l;
};

const struct suffix vsuf_1[] = {
  { "u", 1, 0 }, { "U", 1, 0 },
  { "l", 0, 1 }, { "L", 0, 1 }
};

const struct suffix vsuf_2[] = {
  { "ul", 1, 1 }, { "UL", 1, 1 }, { "uL", 1, 1 }, { "Ul", 1, 1 },
  { "lu", 1, 1 }, { "LU", 1, 1 }, { "Lu", 1, 1 }, { "lU", 1, 1 },
  { "ll", 0, 2 }, { "LL", 0, 2 }
};

const struct suffix vsuf_3[] = {
  { "ull", 1, 2 }, { "ULL", 1, 2 }, { "uLL", 1, 2 }, { "Ull", 1, 2 },
  { "llu", 1, 2 }, { "LLU", 1, 2 }, { "LLu", 1, 2 }, { "llU", 1, 2 }
};
#define Nsuff(tab) (sizeof tab / sizeof (struct suffix))

static struct op
parse_number (pfile, tok)
     cpp_reader *pfile;
     const cpp_token *tok;
{
  struct op op;
  const U_CHAR *start = tok->val.str.text;
  const U_CHAR *end = start + tok->val.str.len;
  const U_CHAR *p = start;
  int c = 0, i, nsuff;
  unsigned HOST_WIDEST_INT n = 0, nd, MAX_over_base;
  int base = 10;
  int overflow = 0;
  int digit, largest_digit = 0;
  const struct suffix *sufftab;

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

  for(; p < end; p++)
    {
      c = *p;

      if (c >= '0' && c <= '9')
	digit = c - '0';
      /* We believe that in all live character sets, a-f are
	 consecutive, and so are A-F.  */
      else if (base == 16 && c >= 'a' && c <= 'f')
	digit = c - 'a' + 10;
      else if (base == 16 && c >= 'A' && c <= 'F')
	digit = c - 'A' + 10;
      else
	break;

      if (largest_digit < digit)
	largest_digit = digit;
      nd = n * base + digit;
      overflow |= MAX_over_base < n || nd < n;
      n = nd;
    }

  if (p < end)
    {
      /* Check for a floating point constant.  Note that float constants
	 with an exponent or suffix but no decimal point are technically
	 invalid (C99 6.4.4.2) but accepted elsewhere.  */
      if ((c == '.' || c == 'F' || c == 'f')
	  || (base == 10 && (c == 'E' || c == 'e')
	      && p+1 < end && (p[1] == '+' || p[1] == '-'))
	  || (base == 16 && (c == 'P' || c == 'p')
	      && p+1 < end && (p[1] == '+' || p[1] == '-')))
	SYNTAX_ERROR ("floating point numbers are not valid in #if");
  
      /* Determine the suffix. l means long, and u means unsigned.
	 See the suffix tables, above.  */
      switch (end - p)
	{
	case 1: sufftab = vsuf_1; nsuff = Nsuff(vsuf_1); break;
	case 2: sufftab = vsuf_2; nsuff = Nsuff(vsuf_2); break;
	case 3: sufftab = vsuf_3; nsuff = Nsuff(vsuf_3); break;
	default: goto invalid_suffix;
	}

      for (i = 0; i < nsuff; i++)
	if (memcmp (p, sufftab[i].s, end - p) == 0)
	  break;
      if (i == nsuff)
	goto invalid_suffix;
      op.unsignedp = sufftab[i].u;

      if (CPP_WTRADITIONAL (pfile)
	  && sufftab[i].u
	  && ! cpp_sys_macro_p (pfile))
	cpp_warning (pfile, "traditional C rejects the `U' suffix");
      if (sufftab[i].l == 2 && CPP_OPTION (pfile, pedantic)
	  && ! CPP_OPTION (pfile, c99))
	cpp_pedwarn (pfile, "too many 'l' suffixes in integer constant");
    }
  
  if (base <= largest_digit)
    cpp_pedwarn (pfile, "integer constant contains digits beyond the radix");

  if (overflow)
    cpp_pedwarn (pfile, "integer constant out of range");

  /* If too big to be signed, consider it unsigned.  */
  else if ((HOST_WIDEST_INT) n < 0 && ! op.unsignedp)
    {
      if (base == 10)
	cpp_warning (pfile, "integer constant is so large that it is unsigned");
      op.unsignedp = 1;
    }

  op.value = n;
  op.op = CPP_INT;
  return op;

 invalid_suffix:
  cpp_error (pfile, "invalid suffix '%.*s' on integer constant",
	     (int) (end - p), p);
 syntax_error:
  op.op = CPP_ERROR;
  return op;
}

static struct op
parse_defined (pfile)
     cpp_reader *pfile;
{
  int paren = 0;
  cpp_hashnode *node = 0;
  cpp_token token;
  struct op op;

  /* Don't expand macros.  */
  pfile->state.prevent_expansion++;

  cpp_get_token (pfile, &token);
  if (token.type == CPP_OPEN_PAREN)
    {
      paren = 1;
      cpp_get_token (pfile, &token);
    }

  if (token.type == CPP_NAME)
    {
      node = token.val.node;
      if (paren)
	{
	  cpp_get_token (pfile, &token);
	  if (token.type != CPP_CLOSE_PAREN)
	    {
	      cpp_error (pfile, "missing ')' after \"defined\"");
	      node = 0;
	    }
	}
    }
  else
    {
      cpp_error (pfile, "operator \"defined\" requires an identifier");
      if (token.flags & NAMED_OP)
	{
	  cpp_token op;

	  op.flags = 0;
	  op.type = token.type;
	  cpp_error (pfile,
		     "(\"%s\" is an alternative token for \"%s\" in C++)",
		     cpp_token_as_text (pfile, &token),
		     cpp_token_as_text (pfile, &op));
	}
    }

  if (!node)
    op.op = CPP_ERROR;
  else
    {
      op.value = node->type == NT_MACRO;
      op.unsignedp = 0;
      op.op = CPP_INT;

      /* No macros?  At top of file?  */
      if (pfile->mi_state == MI_OUTSIDE && pfile->mi_cmacro == 0
	  && pfile->mi_if_not_defined == MI_IND_NOT && pfile->mi_lexed == 1)
	{
	  cpp_start_lookahead (pfile);
	  cpp_get_token (pfile, &token);
	  if (token.type == CPP_EOF)
	    pfile->mi_ind_cmacro = node;
	  cpp_stop_lookahead (pfile, 0);
	}
    }

  pfile->state.prevent_expansion--;
  return op;
}

/* Read one token.  */

static struct op
lex (pfile, skip_evaluation, token)
     cpp_reader *pfile;
     int skip_evaluation;
     cpp_token *token;
{
  struct op op;

  cpp_get_token (pfile, token);

  switch (token->type)
    {
    case CPP_INT:
    case CPP_NUMBER:
      return parse_number (pfile, token);

    case CPP_CHAR:
    case CPP_WCHAR:
      {
	unsigned int chars_seen;

	/* This is always a signed type.  */
	op.unsignedp = 0;
	op.op = CPP_INT;
	op.value = cpp_interpret_charconst (pfile, token, 1, 0, &chars_seen);
	return op;
      }

    case CPP_STRING:
    case CPP_WSTRING:
      SYNTAX_ERROR ("string constants are not valid in #if");

    case CPP_FLOAT:
      SYNTAX_ERROR ("floating point numbers are not valid in #if");

    case CPP_OTHER:
      if (ISGRAPH (token->val.c))
	SYNTAX_ERROR2 ("invalid character '%c' in #if", token->val.c);
      else
	SYNTAX_ERROR2 ("invalid character '\\%03o' in #if", token->val.c);

    case CPP_NAME:
      if (token->val.node == pfile->spec_nodes.n_defined)
	{
	  if (pfile->context->prev && CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "\"defined\" operator appears during macro expansion");

	  return parse_defined (pfile);
	}
      else if (CPP_OPTION (pfile, cplusplus)
	       && (token->val.node == pfile->spec_nodes.n_true
		   || token->val.node == pfile->spec_nodes.n_false))
	{
	  op.op = CPP_INT;
	  op.unsignedp = 0;
	  op.value = (token->val.node == pfile->spec_nodes.n_true);

	  /* Warn about use of true or false in #if when pedantic
	     and stdbool.h has not been included.  */
	  if (CPP_PEDANTIC (pfile)
	      && ! cpp_defined (pfile, DSC("__bool_true_false_are_defined")))
	    cpp_pedwarn (pfile, "ISO C++ does not permit \"%s\" in #if",
			 NODE_NAME (token->val.node));
	  return op;
	}
      else
	{
	  /* Controlling #if expressions cannot contain identifiers (they
	     could become macros in the future).  */
	  pfile->mi_state = MI_FAILED;

	  op.op = CPP_INT;
	  op.unsignedp = 0;
	  op.value = 0;

	  if (CPP_OPTION (pfile, warn_undef) && !skip_evaluation)
	    cpp_warning (pfile, "\"%s\" is not defined",
			 NODE_NAME (token->val.node));
	  return op;
	}

    case CPP_HASH:
      {
	int temp;

	op.op = CPP_INT;
	if (_cpp_test_assertion (pfile, &temp))
	  op.op = CPP_ERROR;
	op.unsignedp = 0;
	op.value = temp;
	return op;
      }

    case CPP_NOT:
      /* We don't worry about its position here.  */
      pfile->mi_if_not_defined = MI_IND_NOT;
      /* Fall through.  */

    default:
      if (((int) token->type > (int) CPP_EQ
	   && (int) token->type < (int) CPP_PLUS_EQ)
	  || token->type == CPP_EOF)
	{
	  op.op = token->type;
	  return op;
	}

      SYNTAX_ERROR2 ("\"%s\" is not valid in #if expressions",
		     cpp_token_as_text (pfile, token));
    }

 syntax_error:
  op.op = CPP_ERROR;
  return op;
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
#define MINMAX_PRIO	   (11 << PRIO_SHIFT)
#define EQUAL_PRIO         (12 << PRIO_SHIFT)
#define LESS_PRIO          (13 << PRIO_SHIFT)
#define SHIFT_PRIO         (14 << PRIO_SHIFT)
#define PLUS_PRIO          (15 << PRIO_SHIFT)
#define MUL_PRIO           (16 << PRIO_SHIFT)
#define UNARY_PRIO        ((17 << PRIO_SHIFT) | RIGHT_ASSOC | NO_L_OPERAND)

/* Operator to priority map.  Must be in the same order as the first
   N entries of enum cpp_ttype.  */
static const short
op_to_prio[] =
{
  /* EQ */		0,		/* dummy entry - can't happen */
  /* NOT */		UNARY_PRIO,
  /* GREATER */		LESS_PRIO,
  /* LESS */		LESS_PRIO,
  /* PLUS */		UNARY_PRIO,	/* note these two can be unary */
  /* MINUS */		UNARY_PRIO,	/* or binary */
  /* MULT */		MUL_PRIO,
  /* DIV */		MUL_PRIO,
  /* MOD */		MUL_PRIO,
  /* AND */		AND_PRIO,
  /* OR */		OR_PRIO,
  /* XOR */		XOR_PRIO,
  /* RSHIFT */		SHIFT_PRIO,
  /* LSHIFT */		SHIFT_PRIO,
  /* MIN */		MINMAX_PRIO,	/* C++ specific */
  /* MAX */		MINMAX_PRIO,	/* extensions */

  /* COMPL */		UNARY_PRIO,
  /* AND_AND */		ANDAND_PRIO,
  /* OR_OR */		OROR_PRIO,
  /* QUERY */		COND_PRIO,
  /* COLON */		COLON_PRIO,
  /* COMMA */		COMMA_PRIO,
  /* OPEN_PAREN */	OPEN_PAREN_PRIO,
  /* CLOSE_PAREN */	CLOSE_PAREN_PRIO,
  /* EQ_EQ */		EQUAL_PRIO,
  /* NOT_EQ */		EQUAL_PRIO,
  /* GREATER_EQ */	LESS_PRIO,
  /* LESS_EQ */		LESS_PRIO
};

#define COMPARE(OP) \
  top->unsignedp = 0; \
  top->value = (unsigned1 | unsigned2) \
  ? (unsigned HOST_WIDEST_INT) v1 OP (unsigned HOST_WIDEST_INT) v2 \
  : (v1 OP v2)
#define EQUALITY(OP) \
  top->value = v1 OP v2; \
  top->unsignedp = 0;
#define BITWISE(OP) \
  top->value = v1 OP v2; \
  top->unsignedp = unsigned1 | unsigned2;
#define MINMAX(OP) \
  top->value = (v1 OP v2) ? v1 : v2; \
  top->unsignedp = unsigned1 | unsigned2;
#define UNARY(OP) \
  top->value = OP v2; \
  top->unsignedp = unsigned2; \
  top->flags |= HAVE_VALUE;
#define SHIFT(PSH, MSH) \
  if (skip_evaluation)  \
    break;		\
  top->unsignedp = unsigned1; \
  if (v2 < 0 && ! unsigned2)  \
    top->value = MSH (pfile, v1, unsigned1, -v2); \
  else \
    top->value = PSH (pfile, v1, unsigned1, v2);

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
  struct op init_stack[INIT_STACK_SIZE];
  struct op *stack = init_stack;
  struct op *limit = stack + INIT_STACK_SIZE;
  cpp_token token;
  register struct op *top = stack + 1;
  int skip_evaluation = 0;
  int result;

  /* Set up detection of #if ! defined().  */
  pfile->mi_lexed = 0;
  pfile->mi_if_not_defined = MI_IND_NONE;

  /* We've finished when we try to reduce this.  */
  top->op = CPP_EOF;
  /* Nifty way to catch missing '('.  */
  top->prio = EXTRACT_PRIO(CLOSE_PAREN_PRIO);
  /* Avoid missing right operand checks.  */
  top->flags = NO_R_OPERAND;

  for (;;)
    {
      unsigned int prio;
      unsigned int flags;
      struct op op;

      /* Read a token */
      op = lex (pfile, skip_evaluation, &token);
      pfile->mi_lexed++;

      /* If the token is an operand, push its value and get next
	 token.  If it is an operator, get its priority and flags, and
	 try to reduce the expression on the stack.  */
      switch (op.op)
	{
	case CPP_ERROR:
	  goto syntax_error;
	push_immediate:
	case CPP_INT:
	  /* Push a value onto the stack.  */
	  if (top->flags & HAVE_VALUE)
	    SYNTAX_ERROR ("missing binary operator");
	  top->value = op.value;
	  top->unsignedp = op.unsignedp;
	  top->flags |= HAVE_VALUE;
	  continue;

	case CPP_EOF:	prio = FORCE_REDUCE_PRIO;	break;
	case CPP_PLUS:
	case CPP_MINUS: prio = PLUS_PRIO;  if (top->flags & HAVE_VALUE) break;
          /* else unary; fall through */
	default:	prio = op_to_prio[op.op];	break;
	}

      /* Separate the operator's code into priority and flags.  */
      flags = EXTRACT_FLAGS(prio);
      prio = EXTRACT_PRIO(prio);
      if (prio == EXTRACT_PRIO(OPEN_PAREN_PRIO))
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
	      if (top->op == CPP_OPEN_PAREN)
		SYNTAX_ERROR ("void expression between '(' and ')'");
	      else
		SYNTAX_ERROR2 ("operator '%s' has no right operand",
			       op_as_text (pfile, top->op));
	    }

	  unsigned2 = top->unsignedp, v2 = top->value;
	  top--;
	  unsigned1 = top->unsignedp, v1 = top->value;

	  /* Now set top->value = (top[1].op)(v1, v2); */
	  switch (top[1].op)
	    {
	    default:
	      cpp_ice (pfile, "impossible operator '%s'",
			       op_as_text (pfile, top[1].op));
	      goto syntax_error;

	    case CPP_NOT:	 UNARY(!);	break;
	    case CPP_COMPL:	 UNARY(~);	break;
	    case CPP_LESS:  	 COMPARE(<);	break;
	    case CPP_GREATER:	 COMPARE(>);	break;
	    case CPP_LESS_EQ:	 COMPARE(<=);	break;
	    case CPP_GREATER_EQ: COMPARE(>=);	break;
	    case CPP_EQ_EQ:	 EQUALITY(==);	break;
	    case CPP_NOT_EQ:	 EQUALITY(!=);	break;
	    case CPP_AND:	 BITWISE(&);	break;
	    case CPP_XOR:	 BITWISE(^);	break;
	    case CPP_OR:	 BITWISE(|);	break;
	    case CPP_LSHIFT:	 SHIFT(left_shift, right_shift); break;
	    case CPP_RSHIFT:	 SHIFT(right_shift, left_shift); break;
	    case CPP_MIN:	 MINMAX(<);	break;
	    case CPP_MAX:	 MINMAX(>);	break;

	    case CPP_PLUS:
	      if (!(top->flags & HAVE_VALUE))
		{
		  /* Can't use UNARY(+) because K+R C did not have unary
		     plus.  Can't use UNARY() because some compilers object
		     to the empty argument.  */
		  top->value = v2;
		  top->unsignedp = unsigned2;
		  top->flags |= HAVE_VALUE;

		  if (CPP_WTRADITIONAL (pfile))
		    cpp_warning (pfile,
			"traditional C rejects the unary plus operator");
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
	    case CPP_MINUS:
	      if (!(top->flags & HAVE_VALUE))
		{
		  UNARY(-);
		  if (!skip_evaluation && (top->value & v2) < 0 && !unsigned2)
		    integer_overflow (pfile);
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
	    case CPP_MULT:
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
	    case CPP_DIV:
	    case CPP_MOD:
	      if (skip_evaluation)
		break;
	      if (v2 == 0)
		SYNTAX_ERROR ("division by zero in #if");
	      top->unsignedp = unsigned1 | unsigned2;
	      if (top[1].op == CPP_DIV)
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

	    case CPP_OR_OR:
	      top->value = v1 || v2;
	      top->unsignedp = 0;
	      if (v1) skip_evaluation--;
	      break;
	    case CPP_AND_AND:
	      top->value = v1 && v2;
	      top->unsignedp = 0;
	      if (!v1) skip_evaluation--;
	      break;
	    case CPP_COMMA:
	      if (CPP_PEDANTIC (pfile))
		cpp_pedwarn (pfile, "comma operator in operand of #if");
	      top->value = v2;
	      top->unsignedp = unsigned2;
	      break;
	    case CPP_QUERY:
	      SYNTAX_ERROR ("syntax error '?' without following ':'");
	    case CPP_COLON:
	      if (top[0].op != CPP_QUERY)
		SYNTAX_ERROR ("syntax error ':' without preceding '?'");
	      top--;
	      if (top->value) skip_evaluation--;
	      top->value = top->value ? v1 : v2;
	      top->unsignedp = unsigned1 | unsigned2;
	      break;
	    case CPP_OPEN_PAREN:
	      if (op.op != CPP_CLOSE_PAREN)
		SYNTAX_ERROR ("missing ')' in expression");
	      op.value = v2;
	      op.unsignedp = unsigned2;
	      goto push_immediate;
	    case CPP_EOF:
	      /* Reducing this dummy operator indicates we've finished.  */
	      if (op.op == CPP_CLOSE_PAREN)
		SYNTAX_ERROR ("missing '(' in expression");
	      goto done;
	    }
	}

      /* Handle short-circuit evaluations.  */
      if (flags & SHORT_CIRCUIT)
	switch (op.op)
	  {
	  case CPP_OR_OR:    if (top->value) skip_evaluation++; break;
	  case CPP_AND_AND:
	  case CPP_QUERY:    if (!top->value) skip_evaluation++; break;
	  case CPP_COLON:
	    if (top[-1].value) /* Was '?' condition true?  */
	      skip_evaluation++;
	    else
	      skip_evaluation--;
	  default:
	    break;
	  }

    skip_reduction:
      /* Check we have a left operand iff we need one.  */
      if (flags & NO_L_OPERAND)
	{
	  if (top->flags & HAVE_VALUE)
	    SYNTAX_ERROR2 ("missing binary operator before '%s'",
			   op_as_text (pfile, top->op));
	}
      else
	{
	  if (!(top->flags & HAVE_VALUE))
	    SYNTAX_ERROR2 ("operator '%s' has no left operand",
			   op_as_text (pfile, top->op));
	}

      /* Check for and handle stack overflow.  */
      top++;
      if (top == limit)
	{
	  struct op *new_stack;
	  int old_size = (char *) limit - (char *) stack;
	  int new_size = 2 * old_size;
	  if (stack != init_stack)
	    new_stack = (struct op *) xrealloc (stack, new_size);
	  else
	    {
	      new_stack = (struct op *) xmalloc (new_size);
	      memcpy (new_stack, stack, old_size);
	    }
	  stack = new_stack;
	  top = (struct op *) ((char *) new_stack + old_size);
	  limit = (struct op *) ((char *) new_stack + new_size);
	}
      
      top->flags = flags;
      top->prio = prio & ~EXTRACT_PRIO(RIGHT_ASSOC);
      top->op = op.op;
    }

 done:
  result = (top[1].value != 0);
  if (top != stack)
    CPP_ICE ("unbalanced stack in #if");
  else if (!(top[1].flags & HAVE_VALUE))
    {
      SYNTAX_ERROR ("#if with no expression");
    syntax_error:
      result = 0;  /* Return 0 on syntax error.  */
    }

  /* Free dynamic stack if we allocated one.  */
  if (stack != init_stack)
    free (stack);
  return result;
}

static const unsigned char *
op_as_text (pfile, op)
     cpp_reader *pfile;
     enum cpp_ttype op;
{
  cpp_token token;

  token.type = op;
  token.flags = 0;
  return cpp_token_as_text (pfile, &token);
}
