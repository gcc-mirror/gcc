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

/* { dg-final { scan-assembler-times "\\\[sp, -16\\\]!" 2 } } */
/* { dg-final { scan-assembler-not "\\\[sp, -32\\\]!" } } */
