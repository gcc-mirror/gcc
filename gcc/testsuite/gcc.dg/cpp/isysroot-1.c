/* { dg-options "-isysroot ${srcdir}/gcc.dg/cpp" } */
/* { dg-do compile  { target *-*-darwin* } } */

#include <stdio.h>
int main()
{
  /* Special stdio.h supplies function foo.  */
  void (*x)(void) = foo;
  return 0;
}
