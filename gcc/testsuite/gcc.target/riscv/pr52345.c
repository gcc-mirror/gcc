/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcbv_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32gcbv_zicond -mabi=ilp32" { target { rv32 } } } */

int f(int a, int b)
{
  int c = a != 0;
  int d = (c!=0|b!=0);
  return d;
}

int h (int a, int b)
{
  int c = (a!=0|b);
  int d = c==0;
  return d;
}

/* { dg-final { scan-assembler-times {\tor} 2 } } */
/* { dg-final { scan-assembler-times {\tsnez} 1 } } */
/* { dg-final { scan-assembler-times {\tseqz} 1 } } */
