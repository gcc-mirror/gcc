/* { dg-do compile } */
/* { dg-options "-O2 -Wno-div-by-zero" } */

int n;

__attribute__ ((noinline, returns_twice)) static int
bar (int)
{
  n /= 0;

  return n;
}

int
foo (int x)
{
  return bar (x);
}
