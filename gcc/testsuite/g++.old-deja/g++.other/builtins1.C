// { dg-do run  }
// { dg-options "-O2" }
// Test whether this builtin minimally works in G++.
// Origin: Kaveh Ghazi Jan 16, 2001
// Copyright (C) 2001 Free Software Foundation.
//

namespace std 
{
  extern "C" void abort (void);
}

int main ()
{
  using namespace std;
  
  if (::__builtin_strlen ("hello") != 5)
    abort ();
  
  return 0;
}
