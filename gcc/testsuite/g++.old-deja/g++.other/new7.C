// { dg-do run  }
// Origin: philip_martin@ntlworld.com

#include <new>

extern "C" void abort();

bool new_flag = false;
bool delete_flag = false;

struct X {
  X()
  {
    throw 1;
  }
  void* operator new ( std::size_t n ) throw ( std::bad_alloc )
  {
    new_flag = true;
    return ::operator new( n );
  }
  void operator delete( void* p, std::size_t n ) throw()
  {
    delete_flag = true;
    ::operator delete( p );
  }
};

int
main()
{
  try
    {
      X* x = new X; // gcc 3.0 fails to call operator delete when X::X throws
    }
  catch ( ... )
    {
    }
  if ( ! new_flag || ! delete_flag )
    ::abort();
}
