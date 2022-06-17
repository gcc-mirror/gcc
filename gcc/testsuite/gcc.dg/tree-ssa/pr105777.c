/* PR middle-end/105777 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "\.MUL_OVERFLOW " "optimized" } } */
/* { dg-final { scan-tree-dump " \\+ 61356675;" "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump " > 122713350" "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump " \\+ 263524915338707880" "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump " > 527049830677415760" "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump " \\+ 51130563" "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump " > 102261126" "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump " \\+ 219604096115589900" "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump " > 439208192231179800" "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump " \\+ 55063683;" "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump " > 110127366" "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump " \\+ 236496718893712200" "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump " > 472993437787424400" "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump " \\+ 46684427" "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump " > 93368854" "optimized" { target int32 } } } */
/* { dg-final { scan-tree-dump " \\+ 200508087757712517" "optimized" { target lp64 } } } */
/* { dg-final { scan-tree-dump " > 401016175515425034" "optimized" { target lp64 } } } */

__attribute__((noipa)) int
foo (int x)
{
  return __builtin_mul_overflow_p (x, 35, 0);
}

__attribute__((noipa)) int
bar (long int x)
{
  return __builtin_mul_overflow_p (x, 35L, 0L);
}

__attribute__((noipa)) int
baz (int x)
{
  return __builtin_mul_overflow_p (42, x, 0);
}

__attribute__((noipa)) int
qux (long int x)
{
  return __builtin_mul_overflow_p (42, x, 0L);
}

__attribute__((noipa)) int
corge (int x)
{
  return __builtin_mul_overflow_p (x, -39, 0);
}

__attribute__((noipa)) int
garply (long int x)
{
  return __builtin_mul_overflow_p (x, -39L, 0L);
}

__attribute__((noipa)) int
grault (int x)
{
  return __builtin_mul_overflow_p (-46, x, 0);
}

__attribute__((noipa)) int
waldo (long int x)
{
  return __builtin_mul_overflow_p (-46, x, 0L);
}
