/* { dg-do compile } */
/* { dg-options "-march=rv32imafdc -mabi=ilp32d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

double foo (int t, double a, double b)
{
  if (t > 0)
    return a;
  else
    return b;
}

/* { dg-final { scan-assembler-not "fsd\t" } } */
