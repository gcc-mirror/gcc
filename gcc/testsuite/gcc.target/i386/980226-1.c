/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options -O2 } */

extern int printf (const char *, ...);
extern double bar (double);

int
baz (double d)
{
  double e = bar (d);
  asm volatile ("" : : : "st");
  return printf ("%lg\n", e);
}
