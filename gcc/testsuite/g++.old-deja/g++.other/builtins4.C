// Test whether this builtin minimally works in G++.
// Origin: Kaveh Ghazi Jan 16, 2001
// Copyright (C) 2001 Free Software Foundation.
//
// Special g++ Options: -O2

namespace std 
{
  extern "C" void abort (void);
  extern "C" int printf (const char *, ...);
}

int main ()
{
  using namespace std;
  
  printf ("hello world\n");
  printf ("\n");
  printf ("%s\n", "hello world");
  printf ("%c", '\n');
  std::printf ("hello world\n");
  std::printf ("\n");
  std::printf ("%s\n", "hello world");
  std::printf ("%c", '\n');
  ::__builtin_printf ("hello world\n");
  ::__builtin_printf ("\n");
  ::__builtin_printf ("%s\n", "hello world");
  ::__builtin_printf ("%c", '\n');
  
  return 0;
}

extern "C"
{
  static int ::printf (const char *, ...)
  {
    std::abort ();
  }
}
