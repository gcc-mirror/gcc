/* PR target/64200 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=atom -mmemcpy-strategy=libcall:-1:align -minline-stringops-dynamically" } */

#include <stdarg.h>

extern void bar(char *x);

void foo (int size, ...)
{
  struct
  {
    char x[size];
  } d;

  va_list ap;
  va_start(ap, size);
  d = va_arg(ap, typeof (d));
  va_end(ap);
  bar(d.x);
}
