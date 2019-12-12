/* PR middle-end/51644 */
/* { dg-do compile } */
/* { dg-options "-Wall -fexceptions" } */
/* { dg-require-effective-target exceptions } */

#include <stdarg.h>

extern void baz (int, va_list) __attribute__ ((__noreturn__));

__attribute__ ((__noreturn__))
void
foo (int s, ...)
{
  va_list ap;
  va_start (ap, s);
  baz (s, ap);
  va_end (ap);
}		/* { dg-bogus "function does return" } */

__attribute__ ((__noreturn__))
void
bar (int s, ...)
{
  va_list ap1;
  va_start (ap1, s);
  {
    va_list ap2;
    va_start (ap2, s);
    baz (s, ap1);
    baz (s, ap2);
    va_end (ap2);
  }
  va_end (ap1);
}		/* { dg-bogus "function does return" } */
