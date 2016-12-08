// PR c++/47263
// PR c++/49260
// { dg-options "-fno-asynchronous-unwind-tables -fno-dwarf2-cfi-asm" }
// { dg-do run { target { c++11 && { ! c++1z } } } }

#include <exception>

int main( void )
{
  std::set_unexpected( []{ throw 0; } );
  try
    {
      []() throw( int ) { throw nullptr; }();	// { dg-warning "deprecated" }
    }
  catch( int )
    { }
}
