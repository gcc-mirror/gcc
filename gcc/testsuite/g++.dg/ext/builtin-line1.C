// __builtin_LINE gets the location where the default argument is expanded.
// { dg-do run }

#include <cassert>
struct Foo
{
  int line;
  Foo( int line = __builtin_LINE() )
    : line( line )
  {}
};

int main()
{
  assert (Foo().line == __LINE__);
  assert ((new Foo)->line == __LINE__);
}
