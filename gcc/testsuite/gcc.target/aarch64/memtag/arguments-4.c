/* { dg-do compile } */
/* { dg-additional-options "-O2 -fno-sanitize=memtag-stack" } */

int use (int * x);

void foo (int n)
{
  int x = 99;
  use (&x);
}

/* { dg-final { scan-assembler-not "\tirg" } } */
/* { dg-final { scan-assembler-not "\tstg" } } */
/* { dg-final { scan-assembler-not "\tst2g" } } */
/* { dg-final { scan-assembler-not "\tsubg" } } */
/* { dg-final { scan-assembler-not "\taddg" } } */
