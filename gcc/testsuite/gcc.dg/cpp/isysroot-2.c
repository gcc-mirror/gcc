/* { dg-options "-isysroot ${srcdir}/gcc.dg/cpp" } */
/* { dg-do compile { target *-*-darwin* } } */

#include <Carbon/Carbon.h>
int main()
{
  /* Special Carbon.h supplies function foo.  */
  void (*x)(void) = foo;
  return 0;
}
