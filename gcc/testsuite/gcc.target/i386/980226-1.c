/* { dg-do compile } */
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
