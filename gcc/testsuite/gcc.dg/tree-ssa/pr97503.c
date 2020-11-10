/* PR tree-optimization/97503 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-mbmi -mlzcnt" { target i?86-*-* x86_64-*-* } } */
/* { dg-final { scan-tree-dump-times "\.CLZ" 2 "optimized" { target { { i?86-*-* x86_64-*-* aarch64-*-* powerpc*-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump-not "__builtin_clz" "optimized" { target { { i?86-*-* x86_64-*-* aarch64-*-* powerpc*-*-*} && lp64 } } } } */
/* { dg-final { scan-tree-dump-not "PHI <" "optimized" { target { { i?86-*-* x86_64-*-* aarch64-*-* powerpc*-*-*} && lp64 } } } } */

int
foo (int x)
{
  return x ? __builtin_clz (x) : 32;
}

int
bar (unsigned long long x)
{
  return x ? __builtin_clzll (x) : 64;
}
