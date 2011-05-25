// PR c++/47263
// { dg-options -std=c++0x }
// { dg-do run }

#include <exception>

int main( void )
{
  std::set_unexpected( []{ throw 0; } );
  try
    {
      []() throw( int ) { throw nullptr; }();
    }
  catch( int )
    { }
}
