// Test whether this builtin minimally works in G++.
// Origin: Kaveh Ghazi Jan 16, 2001
// Copyright (C) 2001 Free Software Foundation.
//
// Special g++ Options: -O2

namespace std 
{
  extern "C" void abort (void);
}

int main ()
{
  using namespace std;
  char f[16];
  
  if (::__builtin_strcpy (f, "hello world") != f
      || __builtin_memcmp (f, "hello world", sizeof ("hello world")))
    abort ();
  
  return 0;
}
