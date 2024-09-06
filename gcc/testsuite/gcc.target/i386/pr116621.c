/* { dg-do run } */
/* { dg-options "-O2" } */

#include <stdarg.h>
#include <string.h>

union S8302
{
  union
  {
    double b;
    int c;
  } a;
  long double d;
  unsigned short int f[5];
};

union S8302 s8302;
extern void check8302va (int i, ...);

int
main (void)
{
  memset (&s8302, '\0', sizeof (s8302));
  s8302.a.b = -221438.250000;
  check8302va (1, s8302);
  return 0;
}

__attribute__((noinline, noclone))
void
check8302va (int z, ...)
{
  union S8302 arg, *p;
  va_list ap;

  __builtin_va_start (ap, z);
  p = &s8302;
  arg = __builtin_va_arg (ap, union S8302);
  if (p->a.b != arg.a.b)
    __builtin_abort ();
  __builtin_va_end (ap);
}
