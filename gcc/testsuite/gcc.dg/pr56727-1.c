/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fPIC" } */
/* { dg-final { scan-assembler-not "@(PLT|plt)" { target i?86-*-* x86_64-*-* powerpc*-*-* } } } */

#define define_func(type) \
  void f_ ## type (type b) { f_ ## type (0); } \
  void __attribute__((noinline, noclone)) f_noinline_ ## type (type b) \
  { f_noinline_ ## type (0); }

define_func(char)
define_func(short)
define_func(int)
define_func(long)

int foo(int n)
{
  return (n == 1 || n == 2) ? 1 : foo(n-1) * foo(n-2);
}

int __attribute__((noinline, noclone)) foo_noinline(int n)
{
  return (n == 1 || n == 2) ? 1 : foo_noinline(n-1) * foo_noinline(n-2);
}
