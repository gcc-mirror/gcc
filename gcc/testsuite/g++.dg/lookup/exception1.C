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
      inline Test() throw( Exception );
      inline Test(int n ) throw( Exception );
    private:
      int i;
  };
}

// This line used to fail because Exception wasn't looked up in the
// right scope.
ns::Test::Test() throw( Exception ) : i( 1 )
{
}

ns::Test::Test( int n ) throw( Exception ) : i( n )
{
}

int main(int argc, char* argv[]) {
  ns::Test test;
}
