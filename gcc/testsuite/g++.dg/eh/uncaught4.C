// PR c++/41174
// { dg-do run }
// { dg-options "-Wno-deprecated" }

#include <exception>

#define assert(E) if (!(E)) __builtin_abort();

struct e {
  e()
  {
    assert( !std::uncaught_exception() );
    try {
      throw 1;
    } catch (int i) {
      assert( !std::uncaught_exception() );
      throw;
    }
  }
};

int main()
{
  try {
    throw e();
  } catch (int i) {
    assert( !std::uncaught_exception() );
  }
  assert( !std::uncaught_exception() );
}
