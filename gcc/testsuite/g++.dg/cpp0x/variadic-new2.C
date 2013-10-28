// { dg-do run }
// { dg-options "-std=c++11" }
// PR c++/32597
#include <assert.h>
#include <new>

template< class... Args > void f( Args... args )
{
  { 
    int x = 17;
    (void)x;
  }

  {
    int y(args...);
    assert(y == 0);
  }

}

int main()
{
   f();
}
