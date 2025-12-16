/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

int use (int *x);

void foo (int n)
{
  int x = 99;
  use (&x);
}

/* { dg-final { scan-assembler-times {\tirg\t} 1 } } */
/* { dg-final { scan-assembler-times {\tstg\t} 2 } } */
/* { dg-final { scan-assembler-not "\taddg" } } */
