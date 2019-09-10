/* PR middle-end/91680 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */
/* { dg-final { scan-tree-dump-times " / " 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times " >> " 3 "forwprop1" } } */

__attribute__((noipa)) unsigned long long
foo (unsigned char x)
{
  unsigned long long q = 1 << x;
  return 256 / q;
}

__attribute__((noipa)) unsigned long long
bar (unsigned char x)
{
  unsigned long long q = 1U << x;
  return 256 / q;
}

__attribute__((noipa)) unsigned long long
baz (unsigned char x, unsigned long long y)
{
  /* This can't be optimized, at least not in C++ and maybe not
     in C89, because for x 31 q is -2147483648ULL, not
     2147483648ULL, and e.g. 2147483648ULL >> 31 is 1, while
     2147483648ULL / -2147483648ULL is 0.  */
  unsigned long long q = 1 << x;
  return y / q;
}

__attribute__((noipa)) unsigned long long
qux (unsigned char x, unsigned long long y)
{
  unsigned long long q = 1U << x;
  return y / q;
}
