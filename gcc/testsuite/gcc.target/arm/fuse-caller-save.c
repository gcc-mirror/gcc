/* { dg-do compile } */
/* { dg-options "-O2 -fipa-ra" } */
/* Testing -fipa-ra optimization option.  */

static int __attribute__((noinline))
bar (int x)
{
  return x + 3;
}

int __attribute__((noinline))
foo (int y)
{
  return y + bar (y);
}

int
main (void)
{
  return !(foo (5) == 13);
}

/* For thumb1, r3 is considered likely spilled, and treated differently in
   ira_build_conflicts, which inhibits the fipa-ra optimization.  */
/* { dg-final { scan-assembler-times "mov\tr\[123\], r0" 1 { target { ! arm_thumb1 } } } } */
