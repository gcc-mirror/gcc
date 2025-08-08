// PR middle-end/121389
// { dg-do compile { target musttail } }
// { dg-options "-fsanitize=address" }

int foo (void);
int bar (void);
int baz (unsigned *);

int
bar (void)
{
  for (int a = 0; a < 420; ++a)
    {
      for (int b = 0; b < 420; ++b)
	{
	  for (int c = 0; c < 420; ++c)
	    {
	      unsigned t;
	      int u = baz (&t);
	      if (u == 42)
		[[gnu::musttail]] return foo ();
	      if (u == -42)
		break;
	      if (u == 16)
		goto l1;
	      if (u == 18)
		goto l2;
	      if (u == 20)
		goto l3;
	    }
	  l3:;
	}
      l2:;
    }
  l1:;
  return 42;
}
