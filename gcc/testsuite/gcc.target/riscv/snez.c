/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" } } */

int f (long long a)
{
  return (a != 2) << 1;
}

/* { dg-final { scan-assembler-times "slli\t" 1 } } */
/* { dg-final { scan-assembler-not "srli\t" } } */
/* { dg-final { scan-assembler-times "snez\t" 1 } } */
/* { dg-final { scan-assembler-not "sext.w\t" } } */

