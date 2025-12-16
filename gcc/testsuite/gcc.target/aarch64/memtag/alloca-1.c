/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

extern int use (int *b);

int foo (int n)
{
  int *b = __builtin_alloca (n);
  int a = use (b);
  return a;
}

/* { dg-final { scan-assembler-times {\tirg\t} 1 } } */
/* { dg-final { scan-assembler-times {\tstg\t...?, \[...?\], 16\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst2g\t...?, \[...?\], 32\n} 2 } } */
