/* PR middle-end/30314 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "\.MUL_OVERFLOW " "optimized" } } */
/* { dg-final { scan-tree-dump " > 122713351" "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump " > 527049830677415760" "optimized" { target lp64 } } } */

int
foo (unsigned int x)
{
  return __builtin_mul_overflow_p (x, 35U, 0U);
}

int
bar (unsigned long int x)
{
  return __builtin_mul_overflow_p (x, 35UL, 0UL);
}
