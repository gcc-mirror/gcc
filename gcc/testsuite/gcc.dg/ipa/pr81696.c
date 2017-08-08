/* { dg-options "-O2 -fdump-ipa-icf-details"  } */

int
main (int argc, char **argv)
{
  __label__ lab4, lab5, lab6;

  void foo (void) { goto lab4; }
  void foo2 (void) { goto lab4; }
  void bar (void) { goto lab5; }
  void baz (void) { goto lab6; }

  if (argc)
    foo ();
  else
    foo2 ();

 lab4:;
  bar ();
 lab5:;
  baz ();
 lab6:;
  return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
