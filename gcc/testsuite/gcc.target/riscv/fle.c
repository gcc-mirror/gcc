/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-march=rv64gc -mabi=lp64d  -fno-finite-math-only -fno-trapping-math -fno-signaling-nans" { target { rv64 } } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -fno-finite-math-only -fno-trapping-math -fno-signaling-nans" { target { rv32 } } } */

long
fle (double x, double y)
{
  return __builtin_islessequal (x, y);
}

/* { dg-final { scan-assembler "\tf(?:gt|le)\\.d\t\[^\n\]*\n" } } */
/* { dg-final { scan-assembler-not "f\[rs\]flags" } } */
