/* PR 2743 */
/* { dg-do compile } */

namespace ns {
  class Exception
  {
  };
}

namespace ns
{
  class Test {
    public:
      inline Test()
#if __cplusplus <= 201402L
      throw( Exception )			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
      ;
      inline Test(int n )
#if __cplusplus <= 201402L
      throw( Exception )			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
      ;
    private:
      int i;
  };
}

// This line used to fail because Exception wasn't looked up in the
// right scope.
ns::Test::Test()
#if __cplusplus <= 201402L
throw( Exception )				// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
: i( 1 )
{
}

ns::Test::Test( int n )
#if __cplusplus <= 201402L
throw( Exception )				// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
: i( n )
{
}

int main(int argc, char* argv[]) {
  ns::Test test;
}
