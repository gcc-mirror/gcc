// Test whether this builtin minimally works in G++.
// Origin: Kaveh Ghazi Jan 16, 2001
// Copyright (C) 2001 Free Software Foundation.
//
// Special g++ Options: -O2

namespace std 
{
  extern "C" void abort (void);
  extern "C" char *strcpy (char *, const char *);
  extern "C" int memcmp (const void *, const void *, __SIZE_TYPE__);
}

int main ()
{
  using namespace std;
  char f[16];
  
  if (strcpy (f, "hello world") != f
      || memcmp (f, "hello world", sizeof ("hello world")))
    abort ();

  if (std::strcpy (f, "bye world") != f
      || memcmp (f, "bye world", sizeof ("bye world")))
    abort ();

  if (::__builtin_strcpy (f, "hello world") != f
      || memcmp (f, "hello world", sizeof ("hello world")))
    abort ();
  
  return 0;
}

extern "C"
{
  static char * ::strcpy (char *, const char *)
  {
    std::abort ();
  }
}
