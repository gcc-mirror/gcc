/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

typedef _BitInt(65535) T;

__attribute__((noipa)) T
f (int n)
{
  T x = 1;
  for (int i = 0; i < n; i++)
    x = x * 3;
  return x;
}

int
main (void)
{
  return (int) f (2) - 9;
}
