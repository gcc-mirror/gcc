/* { dg-do compile { target aarch64*-*-* x86_64-*-* } } */
/* { dg-options "-fdump-rtl-ce1 -O2 -w --param max-rtl-if-conversion-unpredictable-cost=100" } */

typedef struct cpp_reader cpp_reader;
enum cpp_ttype
{
  CPP_EQ =
    0, CPP_NOT, CPP_GREATER, CPP_LESS, CPP_PLUS, CPP_MINUS, CPP_MULT, CPP_DIV,
  CPP_MOD, CPP_AND, CPP_OR, CPP_XOR, CPP_RSHIFT, CPP_LSHIFT, CPP_MIN,
  CPP_MAX, CPP_COMPL, CPP_AND_AND, CPP_OR_OR, CPP_QUERY, CPP_COLON,
  CPP_COMMA, CPP_OPEN_PAREN, CPP_CLOSE_PAREN, CPP_EQ_EQ, CPP_NOT_EQ,
  CPP_GREATER_EQ, CPP_LESS_EQ, CPP_PLUS_EQ, CPP_MINUS_EQ, CPP_MULT_EQ,
  CPP_DIV_EQ, CPP_MOD_EQ, CPP_AND_EQ, CPP_OR_EQ, CPP_XOR_EQ, CPP_RSHIFT_EQ,
  CPP_LSHIFT_EQ, CPP_MIN_EQ, CPP_MAX_EQ, CPP_HASH, CPP_PASTE,
  CPP_OPEN_SQUARE, CPP_CLOSE_SQUARE, CPP_OPEN_BRACE, CPP_CLOSE_BRACE,
  CPP_SEMICOLON, CPP_ELLIPSIS, CPP_PLUS_PLUS, CPP_MINUS_MINUS, CPP_DEREF,
  CPP_DOT, CPP_SCOPE, CPP_DEREF_STAR, CPP_DOT_STAR, CPP_ATSIGN, CPP_NAME,
  CPP_NUMBER, CPP_CHAR, CPP_WCHAR, CPP_OTHER, CPP_STRING, CPP_WSTRING,
  CPP_HEADER_NAME, CPP_COMMENT, CPP_MACRO_ARG, CPP_PADDING, CPP_EOF,
};

static struct op lex (cpp_reader *, int);

struct op
{
  enum cpp_ttype op;
  long value;
};

int
_cpp_parse_expr (pfile)
{
  struct op init_stack[20];
  struct op *stack = init_stack;
  struct op *top = stack + 1;
  int skip_evaluation = 0;
  for (;;)
    {
      struct op op;
      op = lex (pfile, skip_evaluation);
      switch (op.op)
	{
	case CPP_OR_OR:
	  if (top->value)
	    skip_evaluation++;
	  else
	    skip_evaluation--;
	}
    }
}

/* { dg-final { scan-rtl-dump "2 true changes made" "ce1" } } */
