/* { dg-do run { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=used-gpr" } */

struct S { int i; };
__attribute__((const, noinline, noclone))
struct S foo (int x)
{
  struct S s;
  s.i = x;
  return s;
}

int a[2048], b[2048], c[2048], d[2048];
struct S e[2048];

__attribute__((noinline, noclone)) void
bar (void)
{
  int i;
  for (i = 0; i < 1024; i++)
    {
      e[i] = foo (i);
      a[i+2] = a[i] + a[i+1];
      b[10] = b[10] + i;
      c[i] = c[2047 - i];
      d[i] = d[i + 1];
    }
}

int
main ()
{
  int i;
  bar ();
  for (i = 0; i < 1024; i++)
    if (e[i].i != i)
      __builtin_abort ();
  return 0;
}
