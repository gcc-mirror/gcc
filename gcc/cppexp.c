/* Parse C expressions for cpplib.
   Copyright (C) 1987, 1992, 1994, 1995, 1997, 1998, 1999, 2000, 2001,
   2002 Free Software Foundation.
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
static struct op eval_token PARAMS ((cpp_reader *, const cpp_token *));
static struct op *reduce PARAMS ((cpp_reader *, struct op *, enum cpp_ttype));

struct op
{
  enum cpp_ttype op;
  uchar unsignedp;    /* True if value should be treated as unsigned.  */
  HOST_WIDEST_INT value; /* The value logically "right" of op.  */
};

/* Token type abuse.  There is no "error" token, but we can't get
   comments in #if, so we can abuse that token type.  Similarly,
   create unary plus and minus operators.  */
#define CPP_ERROR CPP_COMMENT
#define CPP_UPLUS (CPP_LAST_CPP_OP + 1)
#define CPP_UMINUS (CPP_LAST_CPP_OP + 2)

/* With -O2, gcc appears to produce nice code, moving the error
   message load and subsequent jump completely out of the main path.  */
#define SYNTAX_ERROR(msgid) \
  do { cpp_error (pfile, DL_ERROR, msgid); goto syntax_error; } while(0)
#define SYNTAX_ERROR2(msgid, arg) \
  do { cpp_error (pfile, DL_ERROR, msgid, arg); goto syntax_error; } while(0)

struct suffix
{
  const unsigned char s[4];
  const unsigned char u;
  const unsigned char l;
};

static const struct suffix vsuf_1[] = {
  { "u", 1, 0 }, { "U", 1, 0 },
  { "l", 0, 1 }, { "L", 0, 1 }
};

static const struct suffix vsuf_2[] = {
  { "ul", 1, 1 }, { "UL", 1, 1 }, { "uL", 1, 1 }, { "Ul", 1, 1 },
  { "lu", 1, 1 }, { "LU", 1, 1 }, { "Lu", 1, 1 }, { "lU", 1, 1 },
  { "ll", 0, 2 }, { "LL", 0, 2 }
};

static const struct suffix vsuf_3[] = {
  { "ull", 1, 2 }, { "ULL", 1, 2 }, { "uLL", 1, 2 }, { "Ull", 1, 2 },
  { "llu", 1, 2 }, { "LLU", 1, 2 }, { "LLu", 1, 2 }, { "llU", 1, 2 }
};

/* Parse and convert what is presumably an integer in TOK.  Accepts
   decimal, hex, or octal with or without size suffixes.  Returned op
   is CPP_ERROR on error, otherwise it is a CPP_NUMBER.  */
static struct op
parse_number (pfile, tok)
     cpp_reader *pfile;
     const cpp_token *tok;
{
  struct op op;
  const uchar *start = tok->val.str.text;
  const uchar *end = start + tok->val.str.len;
  const uchar *p = start;
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

      if (ISDIGIT (c)
	  || (base == 16 && ISXDIGIT (c)))
	digit = hex_value (c);
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
	case 1: sufftab = vsuf_1; nsuff = ARRAY_SIZE (vsuf_1); break;
	case 2: sufftab = vsuf_2; nsuff = ARRAY_SIZE (vsuf_2); break;
	case 3: sufftab = vsuf_3; nsuff = ARRAY_SIZE (vsuf_3); break;
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
	cpp_error (pfile, DL_WARNING, "traditional C rejects the `U' suffix");
      if (sufftab[i].l == 2 && CPP_OPTION (pfile, pedantic)
	  && ! CPP_OPTION (pfile, c99))
	cpp_error (pfile, DL_PEDWARN,
		   "too many 'l' suffixes in integer constant");
    }
  
  if (base <= largest_digit)
    cpp_error (pfile, DL_PEDWARN,
	       "integer constant contains digits beyond the radix");

  if (overflow)
    cpp_error (pfile, DL_PEDWARN, "integer constant out of range");

  /* If too big to be signed, consider it unsigned.  */
  else if ((HOST_WIDEST_INT) n < 0 && ! op.unsignedp)
    {
      if (base == 10)
	cpp_error (pfile, DL_WARNING,
		   "integer constant is so large that it is unsigned");
      op.unsignedp = 1;
    }

  op.value = n;
  op.op = CPP_NUMBER;
  return op;

 invalid_suffix:
  cpp_error (pfile, DL_ERROR, "invalid suffix '%.*s' on integer constant",
	     (int) (end - p), p);
 syntax_error:
  op.op = CPP_ERROR;
  return op;
}

/* Handle meeting "defined" in a preprocessor expression.  */
static struct op
parse_defined (pfile)
     cpp_reader *pfile;
{
  int paren = 0;
  cpp_hashnode *node = 0;
  const cpp_token *token;
  struct op op;
  cpp_context *initial_context = pfile->context;

  /* Don't expand macros.  */
  pfile->state.prevent_expansion++;

  token = cpp_get_token (pfile);
  if (token->type == CPP_OPEN_PAREN)
    {
      paren = 1;
      token = cpp_get_token (pfile);
    }

  if (token->type == CPP_NAME)
    {
      node = token->val.node;
      if (paren && cpp_get_token (pfile)->type != CPP_CLOSE_PAREN)
	{
	  cpp_error (pfile, DL_ERROR, "missing ')' after \"defined\"");
	  node = 0;
	}
    }
  else
    {
      cpp_error (pfile, DL_ERROR,
		 "operator \"defined\" requires an identifier");
      if (token->flags & NAMED_OP)
	{
	  cpp_token op;

	  op.flags = 0;
	  op.type = token->type;
	  cpp_error (pfile, DL_ERROR,
		     "(\"%s\" is an alternative token for \"%s\" in C++)",
		     cpp_token_as_text (pfile, token),
		     cpp_token_as_text (pfile, &op));
	}
    }

  if (!node)
    op.op = CPP_ERROR;
  else
    {
      if (pfile->context != initial_context)
	cpp_error (pfile, DL_WARNING,
		   "this use of \"defined\" may not be portable");

      op.value = node->type == NT_MACRO;
      op.unsignedp = 0;
      op.op = CPP_NUMBER;

      /* A possible controlling macro of the form #if !defined ().
	 _cpp_parse_expr checks there was no other junk on the line.  */
      pfile->mi_ind_cmacro = node;
    }

  pfile->state.prevent_expansion--;
  return op;
}

/* Convert a token into a CPP_NUMBER (an interpreted preprocessing
   number or character constant, or the result of the "defined" or "#"
   operators), or CPP_ERROR on error.  */
static struct op
eval_token (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  unsigned int temp;
  struct op op;

  op.op = CPP_NUMBER;
  op.unsignedp = 0;

  switch (token->type)
    {
    case CPP_NUMBER:
      return parse_number (pfile, token);

    case CPP_WCHAR:
      op.unsignedp = WCHAR_UNSIGNED;
    case CPP_CHAR:		/* Always unsigned.  */
      op.value = cpp_interpret_charconst (pfile, token, 1, &temp);
      break;

    case CPP_NAME:
      if (token->val.node == pfile->spec_nodes.n_defined)
	return parse_defined (pfile);
      else if (CPP_OPTION (pfile, cplusplus)
	       && (token->val.node == pfile->spec_nodes.n_true
		   || token->val.node == pfile->spec_nodes.n_false))
	{
	  op.value = (token->val.node == pfile->spec_nodes.n_true);

	  /* Warn about use of true or false in #if when pedantic
	     and stdbool.h has not been included.  */
	  if (CPP_PEDANTIC (pfile)
	      && ! cpp_defined (pfile, DSC("__bool_true_false_are_defined")))
	    cpp_error (pfile, DL_PEDWARN,
		       "ISO C++ does not permit \"%s\" in #if",
		       NODE_NAME (token->val.node));
	}
      else
	{
	  op.value = 0;
	  if (CPP_OPTION (pfile, warn_undef) && !pfile->state.skip_eval)
	    cpp_error (pfile, DL_WARNING, "\"%s\" is not defined",
		       NODE_NAME (token->val.node));
	}
      break;

    default: /* CPP_HASH */
      if (_cpp_test_assertion (pfile, &temp))
	op.op = CPP_ERROR;
      op.value = temp;
    }

  return op;
}

/* Warn if appropriate on overflow.  */
static void
integer_overflow (pfile)
     cpp_reader *pfile;
{
  if (CPP_PEDANTIC (pfile))
    cpp_error (pfile, DL_PEDWARN,
	       "integer overflow in preprocessor expression");
}

/* Handle shifting A left by B bits.  UNSIGNEDP is non-zero if A is
   unsigned.  */
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

/* Handle shifting A right by B bits.  UNSIGNEDP is non-zero if A is
   unsigned.  */
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
than the operator on the top of the stack, we reduce the stack by one
operator and repeat the test.  Since equal priorities do not reduce,
this is naturally right-associative.

We handle left-associative operators by decrementing the priority of
just-lexed operators by one, but retaining the priority of operators
already on the stack.

The remaining cases are '(' and ')'.  We handle '(' by skipping the
reduction phase completely.  ')' is given lower priority than
everything else, including '(', effectively forcing a reduction of the
parenthesised expression.  If there is a matching '(', the routine
reduce() exits immediately.  If the normal exit route sees a ')', then
there cannot have been a matching '(' and an error message is output.

The parser assumes all shifted operators require a left operand unless
the flag NO_L_OPERAND is set.  These semantics are automatic; any
extra semantics need to be handled with operator-specific code.  */

/* Flags.  */
#define NO_L_OPERAND	(1 << 0)
#define LEFT_ASSOC	(1 << 1)

/* Operator to priority map.  Must be in the same order as the first
   N entries of enum cpp_ttype.  */
static const struct operator
{
  uchar prio;
  uchar flags;
} optab[] =
{
  /* EQ */		{0, 0},		/* Shouldn't happen.  */
  /* NOT */		{16, NO_L_OPERAND},
  /* GREATER */		{12, LEFT_ASSOC},
  /* LESS */		{12, LEFT_ASSOC},
  /* PLUS */		{14, LEFT_ASSOC},
  /* MINUS */		{14, LEFT_ASSOC},
  /* MULT */		{15, LEFT_ASSOC},
  /* DIV */		{15, LEFT_ASSOC},
  /* MOD */		{15, LEFT_ASSOC},
  /* AND */		{9, LEFT_ASSOC},
  /* OR */		{7, LEFT_ASSOC},
  /* XOR */		{8, LEFT_ASSOC},
  /* RSHIFT */		{13, LEFT_ASSOC},
  /* LSHIFT */		{13, LEFT_ASSOC},
  /* MIN */		{10, LEFT_ASSOC},	/* C++ specific */
  /* MAX */		{10, LEFT_ASSOC},	/* extensions */

  /* COMPL */		{16, NO_L_OPERAND},
  /* AND_AND */		{6, LEFT_ASSOC},
  /* OR_OR */		{5, LEFT_ASSOC},
  /* QUERY */		{3, 0},
  /* COLON */		{4, LEFT_ASSOC},
  /* COMMA */		{2, LEFT_ASSOC},
  /* OPEN_PAREN */	{1, NO_L_OPERAND},
  /* CLOSE_PAREN */	{0, 0},
  /* EOF */		{0, 0},
  /* EQ_EQ */		{11, LEFT_ASSOC},
  /* NOT_EQ */		{11, LEFT_ASSOC},
  /* GREATER_EQ */	{12, LEFT_ASSOC},
  /* LESS_EQ */		{12, LEFT_ASSOC},
  /* UPLUS */		{16, NO_L_OPERAND},
  /* UMINUS */		{16, NO_L_OPERAND}
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
  top->unsignedp = unsigned2;
#define SHIFT(PSH, MSH) \
  if (pfile->state.skip_eval)  \
    break;		\
  top->unsignedp = unsigned1; \
  if (v2 < 0 && ! unsigned2)  \
    top->value = MSH (pfile, v1, unsigned1, -v2); \
  else \
    top->value = PSH (pfile, v1, unsigned1, v2);

/* Parse and evaluate a C expression, reading from PFILE.
   Returns the truth value of the expression.  

   The implementation is an operator precedence parser, i.e. a
   bottom-up parser, using a stack for not-yet-reduced tokens.

   The stack base is op_stack, and the current stack pointer is 'top'.
   There is a stack element for each operator (only), and the most
   recently pushed operator is 'top->op'.  An operand (value) is
   stored in the 'value' field of the stack element of the operator
   that precedes it.  */
bool
_cpp_parse_expr (pfile)
     cpp_reader *pfile;
{
  struct op *top = pfile->op_stack;
  const cpp_token *token = NULL, *prev_token;
  unsigned int lex_count;
  bool saw_leading_not, want_value = true;

  pfile->state.skip_eval = 0;

  /* Set up detection of #if ! defined().  */
  pfile->mi_ind_cmacro = 0;
  saw_leading_not = false;
  lex_count = 0;

  /* Lowest priority operator prevents further reductions.  */
  top->op = CPP_EOF;

  for (;;)
    {
      struct op op;

      prev_token = token;
      token = cpp_get_token (pfile);
      lex_count++;
      op.op = token->type;

      switch (op.op)
	{
	  /* These tokens convert into values.  */
	case CPP_NUMBER:
	case CPP_CHAR:
	case CPP_WCHAR:
	case CPP_NAME:
	case CPP_HASH:
	  if (!want_value)
	    SYNTAX_ERROR2 ("missing binary operator before token \"%s\"",
			   cpp_token_as_text (pfile, token));
	  want_value = false;
	  op = eval_token (pfile, token);
	  if (op.op == CPP_ERROR)
	    goto syntax_error;
	  top->value = op.value;
	  top->unsignedp = op.unsignedp;
	  continue;

	case CPP_NOT:
	  saw_leading_not = lex_count == 1;
	  break;
	case CPP_PLUS:
	  if (want_value)
	    op.op = CPP_UPLUS;
	  break;
	case CPP_MINUS:
	  if (want_value)
	    op.op = CPP_UMINUS;
	  break;
	case CPP_OTHER:
	  if (ISGRAPH (token->val.c))
	    SYNTAX_ERROR2 ("invalid character '%c' in #if", token->val.c);
	  else
	    SYNTAX_ERROR2 ("invalid character '\\%03o' in #if", token->val.c);

	default:
	  if ((int) op.op <= (int) CPP_EQ || (int) op.op >= (int) CPP_PLUS_EQ)
	    SYNTAX_ERROR2 ("token \"%s\" is not valid in #if expressions",
			   cpp_token_as_text (pfile, token));
	  break;
	}

      /* Check we have a value or operator as appropriate.  */
      if (optab[op.op].flags & NO_L_OPERAND)
	{
	  if (!want_value)
	    SYNTAX_ERROR2 ("missing binary operator before token \"%s\"",
			   cpp_token_as_text (pfile, token));
	}
      else if (want_value)
	{
	  /* Ordering here is subtle and intended to favour the
	     missing parenthesis diagnostics over alternatives.  */
	  if (op.op == CPP_CLOSE_PAREN)
	    {
	      if (top->op == CPP_OPEN_PAREN)
		SYNTAX_ERROR ("void expression between '(' and ')'");
	    }
	  else if (top->op == CPP_EOF)
	    SYNTAX_ERROR ("#if with no expression");
	  if (top->op != CPP_EOF && top->op != CPP_OPEN_PAREN)
	    SYNTAX_ERROR2 ("operator '%s' has no right operand",
			   cpp_token_as_text (pfile, prev_token));
	}

      top = reduce (pfile, top, op.op);
      if (!top)
	goto syntax_error;

      if (op.op == CPP_EOF)
	break;

      switch (op.op)
	{
	case CPP_CLOSE_PAREN:
	  continue;
	case CPP_OR_OR:
	  if (top->value)
	    pfile->state.skip_eval++;
	  break;
	case CPP_AND_AND:
	case CPP_QUERY:
	  if (!top->value)
	    pfile->state.skip_eval++;
	  break;
	case CPP_COLON:
	  if (top->op != CPP_QUERY)
	    SYNTAX_ERROR (" ':' without preceding '?'");
	  if (top[-1].value) /* Was '?' condition true?  */
	    pfile->state.skip_eval++;
	  else
	    pfile->state.skip_eval--;
	default:
	  break;
	}

      want_value = true;

      /* Check for and handle stack overflow.  */
      if (++top == pfile->op_limit)
	top = _cpp_expand_op_stack (pfile);
      
      top->op = op.op;
    }

  /* The controlling macro expression is only valid if we called lex 3
     times: <!> <defined expression> and <EOF>.  push_conditional ()
     checks that we are at top-of-file.  */
  if (pfile->mi_ind_cmacro && !(saw_leading_not && lex_count == 3))
    pfile->mi_ind_cmacro = 0;

  if (top != pfile->op_stack)
    {
      cpp_error (pfile, DL_ICE, "unbalanced stack in #if");
    syntax_error:
      return false;  /* Return false on syntax error.  */
    }

  return top->value != 0;
}

/* Reduce the operator / value stack if possible, in preparation for
   pushing operator OP.  Returns NULL on error, otherwise the top of
   the stack.  */
static struct op *
reduce (pfile, top, op)
     cpp_reader *pfile;
     struct op *top;
     enum cpp_ttype op;
{
  unsigned int prio;

  if (op == CPP_OPEN_PAREN)
    return top;

  /* Decrement the priority of left-associative operators to force a
     reduction with operators of otherwise equal priority.  */
  prio = optab[op].prio - ((optab[op].flags & LEFT_ASSOC) != 0);
  while (prio < optab[top->op].prio)
    {
      HOST_WIDEST_INT v1, v2;
      unsigned int unsigned1, unsigned2;

      unsigned2 = top->unsignedp, v2 = top->value;
      top--;
      unsigned1 = top->unsignedp, v1 = top->value;

      /* Now set top->value = (top[1].op)(v1, v2); */
      switch (top[1].op)
	{
	default:
	  cpp_error (pfile, DL_ICE, "impossible operator '%u'", top[1].op);
	  return 0;

	case CPP_NOT:	 UNARY(!);	break;
	case CPP_COMPL:	 UNARY(~);	break;
	case CPP_LESS: 	 COMPARE(<);	break;
	case CPP_GREATER: COMPARE(>);	break;
	case CPP_LESS_EQ: COMPARE(<=);	break;
	case CPP_GREATER_EQ: COMPARE(>=); break;
	case CPP_EQ_EQ:	 EQUALITY(==);	break;
	case CPP_NOT_EQ: EQUALITY(!=);	break;
	case CPP_AND:	 BITWISE(&);	break;
	case CPP_XOR:	 BITWISE(^);	break;
	case CPP_OR:	 BITWISE(|);	break;
	case CPP_LSHIFT: SHIFT(left_shift, right_shift); break;
	case CPP_RSHIFT: SHIFT(right_shift, left_shift); break;
	case CPP_MIN:	 MINMAX(<);	break;
	case CPP_MAX:	 MINMAX(>);	break;

	case CPP_UPLUS:
	  /* Can't use UNARY(+) because K+R C did not have unary
	     plus.  Can't use UNARY() because some compilers object
	     to the empty argument.  */
	  top->value = v2;
	  top->unsignedp = unsigned2;
	  if (CPP_WTRADITIONAL (pfile))
	    cpp_error (pfile, DL_WARNING,
		       "traditional C rejects the unary plus operator");
	  break;
	case CPP_UMINUS:
	  UNARY(-);
	  if (!pfile->state.skip_eval && (top->value & v2) < 0 && !unsigned2)
	    integer_overflow (pfile);
	  break;

	case CPP_PLUS:
	  top->value = v1 + v2;
	  top->unsignedp = unsigned1 | unsigned2;
	  if (! top->unsignedp && ! pfile->state.skip_eval
	      && ! possible_sum_sign (v1, v2, top->value))
	    integer_overflow (pfile);
	  break;
	case CPP_MINUS:
	  top->value = v1 - v2;
	  top->unsignedp = unsigned1 | unsigned2;
	  if (! top->unsignedp && ! pfile->state.skip_eval
	      && ! possible_sum_sign (top->value, v2, v1))
	    integer_overflow (pfile);
	  break;
	case CPP_MULT:
	  top->unsignedp = unsigned1 | unsigned2;
	  if (top->unsignedp)
	    top->value = (unsigned HOST_WIDEST_INT) v1 * v2;
	  else if (!pfile->state.skip_eval)
	    {
	      top->value = v1 * v2;
	      if (v1 && (top->value / v1 != v2
			 || (top->value & v1 & v2) < 0))
		integer_overflow (pfile);
	    }
	  break;
	case CPP_DIV:
	case CPP_MOD:
	  if (pfile->state.skip_eval)
	    break;
	  if (v2 == 0)
	    {
	      cpp_error (pfile, DL_ERROR, "division by zero in #if");
	      return 0;
	    }
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
	  if (v1) pfile->state.skip_eval--;
	  break;
	case CPP_AND_AND:
	  top->value = v1 && v2;
	  top->unsignedp = 0;
	  if (!v1) pfile->state.skip_eval--;
	  break;
	case CPP_COMMA:
	  if (CPP_PEDANTIC (pfile))
	    cpp_error (pfile, DL_PEDWARN,
		       "comma operator in operand of #if");
	  top->value = v2;
	  top->unsignedp = unsigned2;
	  break;
	case CPP_QUERY:
	  cpp_error (pfile, DL_ERROR, "'?' without following ':'");
	  return 0;
	case CPP_COLON:
	  top--;
	  if (top->value) pfile->state.skip_eval--;
	  top->value = top->value ? v1 : v2;
	  top->unsignedp = unsigned1 | unsigned2;
	  break;
	case CPP_OPEN_PAREN:
	  if (op != CPP_CLOSE_PAREN)
	    {
	      cpp_error (pfile, DL_ERROR, "missing ')' in expression");
	      return 0;
	    }
	  top->value = v2;
	  top->unsignedp = unsigned2;
	  return top;
	}
    }

  if (op == CPP_CLOSE_PAREN)
    {
      cpp_error (pfile, DL_ERROR, "missing '(' in expression");
      return 0;
    }

  return top;
}

/* Returns the position of the old top of stack after expansion.  */
struct op *
_cpp_expand_op_stack (pfile)
     cpp_reader *pfile;
{
  size_t n = (size_t) (pfile->op_limit - pfile->op_stack);

  pfile->op_stack = (struct op *) xrealloc (pfile->op_stack,
					    (n * 2 + 20) * sizeof (struct op));

  return pfile->op_stack + n;
}
