/* { dg-do run } */
/* { dg-options "-msave-restore" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int
__attribute__((noinline,noclone))
foo (int u)
{
  return u + 1;
}

int
__attribute__((noinline,noclone))
bar (int a, int b, int c, int d, int e, int f, int g, int h, int u)
{
  return foo (u);
}

int main()
{
  if (bar (1, 2, 3, 4, 5, 6, 7, 8, 9) != 10)
    __builtin_abort();
  return 0;
}
