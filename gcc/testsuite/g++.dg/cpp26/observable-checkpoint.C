// P1494R5+P3641R0 - Partial program correctness.
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdump-tree-optimized -Wno-return-type -O" }
// { dg-final { scan-tree-dump  {\+\s42} "optimized" } }
// { dg-final { scan-tree-dump  {__builtin_observable_checkpoint} "optimized" } }

extern int x;

int
here_b_ub ()
{
  // missing return triggers UB (we must ignore the warning for this test). 
}

int
main ()
{
  x = 0;
  __builtin_printf (" start \n");
  x += 42;
  // Without this checkpoint the addition above is elided (along with the rest
  // of main).
  __builtin_observable_checkpoint ();
  here_b_ub ();
}
