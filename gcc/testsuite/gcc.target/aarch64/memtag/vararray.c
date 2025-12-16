/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

extern int *use (int *b, int n);

int *foo (int n)
{
  int b[n];
  return use (b, n);
}

/* { dg-final { scan-assembler-times {\tirg\t} 1 } } */
/* { dg-final { scan-assembler-times {stg\t...?, \[...?\], 16\n} 3 } } */
/* { dg-final { scan-assembler-times {st2g\t...?, \[...?\], 32\n} 3 } } */
/* { dg-final { scan-assembler-times {\taddg\t} 0 } } */
