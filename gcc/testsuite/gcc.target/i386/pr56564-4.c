/* PR target/56564 */
/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O3 -fno-pic -fdump-tree-optimized" } */

__thread struct S { long a, b; } s = { 5, 6 };
__thread char t[16] = { 7 };

int
foo (void)
{
  return ((__UINTPTR_TYPE__) &s) & 15;
}

int
bar (void)
{
  return ((__UINTPTR_TYPE__) &t[0]) & 15;
}

/* { dg-final { scan-assembler-not ".align\[ \t]*16\[^:]*\[\n\r]s:" { target { *-*-linux* } } } } */
/* { dg-final { scan-assembler ".align\[ \t]*16\[^:]*\[\n\r]t:" { target { *-*-linux* } } } } */
