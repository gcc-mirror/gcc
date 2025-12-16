/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

int use (int *x);

__attribute__((no_sanitize("memtag-stack")))
void foo (int n)
{
  int x = 99;
  use (&x);
}

/* { dg-final { scan-assembler-not "\tirg" } } */
/* { dg-final { scan-assembler-not "\tgmi" } } */
/* { dg-final { scan-assembler-not "\tstg" } } */
/* { dg-final { scan-assembler-not "\tst2g" } } */
/* { dg-final { scan-assembler-not "\tsubg" } } */
/* { dg-final { scan-assembler-not "\taddg" } } */
