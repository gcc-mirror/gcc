/* Test demangling a C++ function.  */
/* { dg-do run } */

#include <cstring>
#include <cstdlib>
#include <iostream>

class demangle_test
{
public:
  /* Return 0 if the demangling test succeeds.  */
  static int test_function1 ()
  {
    std::cout << __PRETTY_FUNCTION__ << "\n";
    return std::strcmp (__PRETTY_FUNCTION__, "static int demangle_test::test_function1()");
  }

  /* Return 0 if the demangling test succeeds.  */
  static int test_function2 (int ignored)
  {
    std::cout << __PRETTY_FUNCTION__ << "\n";
    return std::strcmp (__PRETTY_FUNCTION__, "static int demangle_test::test_function2(int)");
  }
};

int main ()
{
  if (demangle_test::test_function1 () != 0)
    abort ();

  if (demangle_test::test_function2 (0) != 0)
    abort ();
  
  return 0;
}
