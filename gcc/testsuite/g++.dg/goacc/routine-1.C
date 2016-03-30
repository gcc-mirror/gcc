/* Test valid use of the routine directive.  */

namespace N
{
  extern void foo1();
  extern void foo2();
#pragma acc routine (foo1)
#pragma acc routine
  void foo3()
  {
  }
}
#pragma acc routine (N::foo2)
