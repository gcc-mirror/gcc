/* { dg-do compile  { target { ia32 } } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler-not "%ebp" } } */

__attribute__((__noinline__, __noclone__, __stdcall__)) void g(int a)
{
  __builtin_printf("in g(): %d\n", a);
}

__attribute__((__noinline__, __noclone__, __thiscall__)) void h(int a, int b)
{
  __builtin_printf("in h(): %d %d\n", a, b);
}

void f()
{
  g(0);
  h(0, 1);
  __builtin_puts("in f()");
}
