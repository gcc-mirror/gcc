// PR c++/47263
// PR c++/49260
// { dg-options "-std=c++0x -fno-asynchronous-unwind-tables -fno-dwarf2-cfi-asm" }
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
