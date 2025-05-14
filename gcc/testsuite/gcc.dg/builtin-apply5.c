/* { dg-options "-O2 -Wmissing-noreturn -fgnu89-inline" } */
/* { dg-additional-options "-mno-mmx" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-do compile } */

extern void abort (void);

double
foo (int arg)
{
  if (arg != 116)
    abort();
  return arg + 1;
}

__attribute__((noreturn))
double
bar (int arg)
{
  foo (arg);
  __builtin_return (__builtin_apply ((void (*) ()) foo, /* { dg-warning "'noreturn' function does return" } */
				     __builtin_apply_args (), 16));
}

