/* { dg-options "-isysroot ${srcdir}/gcc.dg/cpp" } */
/* { dg-do compile  { target *-*-darwin* } } */

/* For the test to succeed there needs to be some header that is to be found
   in the 'expected' place i.e. <sysroot>/usr/include/.  It's important that
   it is not the name of a header for which fixincludes have been applied,
   since such headers will be found in the gcc include-fixed dir and, in
   general, reference additional headers.  The dummy sysroot will prevent the
   additional headers from being found, resulting in a failed test.  So use
   a header name we don't expect to see. */
#include <example.h>
int main()
{
  /* Special example.h supplies function foo.  */
  void (*x)(void) = foo;
  return 0;
}
