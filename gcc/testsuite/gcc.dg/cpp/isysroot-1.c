/* { dg-options "-isysroot ${srcdir}/gcc.dg/cpp" } */
/* { dg-do compile } */

#include <stdio.h>
int main()
{
  /* Special stdio.h supplies function foo.  */
  void (*x)(void) = foo;
  return 0;
}
