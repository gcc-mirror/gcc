/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" } } */

long long f(int a, int b)
{
  return (a * 3) / b;
}

/* { dg-final { scan-assembler-times "sh1add\t" 1 } } */
/* { dg-final { scan-assembler-times "divw\t" 1 } } */
