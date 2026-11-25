/* PR target/128921 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -fdump-tree-optimized -Wno-psabi" } */

/* The struct is passed in memory, so va_arg must fetch it from the
   overflow area without looking at the register save area.  */

#include <stdarg.h>

struct B { int b : 5; };
struct __attribute__((packed)) S { char a; struct B s; };

char
f (int n, ...)
{
  va_list ap;
  va_start (ap, n);
  struct S s = va_arg (ap, struct S);
  va_end (ap);
  return s.a;
}

/* { dg-final { scan-tree-dump-not "gp_offset" "optimized" } } */
