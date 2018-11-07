/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-loop2_invariant" } */
/* { dg-skip-if "unexpected IV" { "hppa*-*-* mips*-*-* visium-*-* powerpc*-*-* riscv*-*-*" } } */
/* Load immediate on condition is available from z13 on and prevents moving
   the load out of the loop, so always run this test with -march=zEC12 that
   does not have load immediate on condition.  */
/* { dg-additional-options "-march=zEC12" { target { s390*-*-* } } } */

void
f (int *a, int *b)
{
  int i;

  for (i = 0; i < 100; i++)
    {
      int d = 42;

      a[i] = d;
      if (i % 2)
	d = i;
      b[i] = d;
    }
}

/* Load of 42 is moved out of the loop, introducing a new pseudo register.  */
/* { dg-final { scan-rtl-dump-times "Decided" 1 "loop2_invariant" } } */
/* { dg-final { scan-rtl-dump-not "without introducing a new temporary register" "loop2_invariant" } } */

