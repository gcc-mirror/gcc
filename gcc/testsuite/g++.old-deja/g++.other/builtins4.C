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
  
  ::__builtin_printf ("hello world\n");
  ::__builtin_printf ("\n");
  ::__builtin_printf ("%s\n", "hello world");
  ::__builtin_printf ("%c", '\n');
  
  return 0;
}

