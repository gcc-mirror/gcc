/* PR tree-optimization/77664 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1-details" } */

extern void foo (void);

/* { dg-final { scan-tree-dump-times "Optimizing range test \[^\n\r]* and comparison" 6 "reassoc1" } } */

__attribute__((noinline, noclone)) void
fn1 (long long int a, unsigned short b, int c)
{
  if (a >= 0 && c && a < b)
    foo ();
}

__attribute__((noinline, noclone)) void
fn2 (long long int a, unsigned short b, int c)
{
  if (a < 0 || c || a >= b)
    foo ();
}

__attribute__((noinline, noclone)) void
fn3 (long long int a, unsigned short b)
{
  if (a < 0 || b < a)
    foo ();
}

__attribute__((noinline, noclone)) void
fn4 (long long int a, unsigned short b)
{
  if (a <= b && a >= 0)
    foo ();
}

__attribute__((noinline, noclone)) void
fn5 (long long int a, unsigned short b)
{
  if (a < 0 | a > b)
    foo ();
}

__attribute__((noinline, noclone)) void
fn6 (long long int a, unsigned short b)
{
  if (b >= a & a >= 0)
    foo ();
}
