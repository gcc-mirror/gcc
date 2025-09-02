// PR middle-end/121389
// { dg-do compile { target musttail } }
// { dg-options "-fsanitize=address" }

int foo (void);
int bar (void);
int baz (unsigned *);

int
bar (void)
{
  do
    {
      unsigned t;
      int u = baz (&t);
      if (u == 42)
	[[gnu::musttail]] return foo ();
      if (u == -42)
	break;
    }
  while (1);
  return 42;
}
