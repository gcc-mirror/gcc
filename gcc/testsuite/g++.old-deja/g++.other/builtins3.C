// Test whether this builtin minimally works in G++.
// Origin: Kaveh Ghazi Jan 16, 2001
// Copyright (C) 2001 Free Software Foundation.
//
// Special g++ Options: -O2

namespace std 
{
  extern "C" void abort (void);
  extern "C" void *alloca (__SIZE_TYPE__);
}

int main ()
{
  using namespace std;
  void *foo;
  
  foo = alloca (32);
  if (!foo)
    abort ();

  foo = std::alloca (32);
  if (!foo)
    abort ();

  foo = ::__builtin_alloca (32);
  if (!foo)
    abort ();

  return 0;
}

extern "C"
{
  static void * ::alloca (__SIZE_TYPE__)
  {
    std::abort ();
  }
}
