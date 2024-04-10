/* { dg-do run } */
/* { dg-additional-options "-mtune=k8 -mstringop-strategy=rep_8byte" { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } */

struct S { unsigned a[10]; unsigned y; unsigned b[6]; } g[2];

__attribute__((noinline, noclone)) int
test (int x)
{
  struct S e[2] = { g[0], g[1] };
  int r = 0;
  if (x >= 0)
    {
      r++;
      e[1].y++;
    }
  g[1] = e[1];
  return r;
}

int
main ()
{
  test (1);
  if (g[1].y != 1)
    __builtin_abort ();
  return 0;
}
