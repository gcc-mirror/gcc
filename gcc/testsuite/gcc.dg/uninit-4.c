/* Spurious uninit variable warnings, case 4.
   Simplified version of cppexp.c (cpp_parse_expr).

   This one is really fragile, it gets it right if you take out case
   1, or if the structure is replaced by an int, or if the structure
   has fewer members (!) */

/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

struct operation {
    short op;
    char rprio;
    char flags;
    char unsignedp;
    long value;
};

extern struct operation cpp_lex (void);

void
cpp_parse_expr (void)
{
  int rprio; /* { dg-bogus "rprio" "uninitialized variable warning" { xfail *-*-* } } */
  struct operation op;

  for (;;)
    {
      op = cpp_lex ();

      switch (op.op)
	{
	case 0:
	  break;
	case 1:
	  return;
	case 2:
	  rprio = 1;
	  break;
	default:
	  return;
	}

      if (op.op == 0)
	return;

      if (rprio != 1)
	abort();
    }
}
