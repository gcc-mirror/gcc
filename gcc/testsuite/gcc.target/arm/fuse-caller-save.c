/* { dg-do compile } */
/* { dg-options "-O2 -fuse-caller-save" } */
/* Testing -fuse-caller-save optimization option.  */

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
   ira_build_conflicts, which inhibits the fuse-caller-save optimization.  */
/* { dg-final { scan-assembler-times "mov\tr3, r0" 1 { target { ! arm_thumb1 } } } } */
