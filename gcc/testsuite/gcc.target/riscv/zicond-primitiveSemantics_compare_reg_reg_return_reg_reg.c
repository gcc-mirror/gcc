/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d -mtune=generic" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f -mtune=generic" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" "-O3" } } */

#define N 10000

int primitiveSemantics_compare_reg_reg_return_reg_reg_00(int *a, int min_v)
{
  int last = 0;

  for (int i = 0; i < N; i++)
  {
    if (a[i] < min_v)
      last = a[i];
  }
  return last;
}

/* { dg-final { scan-assembler-times {\mczero\.nez\M} 1 } } */
/* { dg-final { scan-assembler-times {\mczero\.eqz\M} 1 } } */
