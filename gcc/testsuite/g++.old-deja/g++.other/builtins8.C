// Test that built-in functions are recognized with a prototype.
// Origin: Roger Sayle  Mar 20, 2002
// Copyright (C) 2002 Free Software Foundation.
//
// Special g++ Options: -O2


extern "C" void link_error (void);

namespace std {
typedef __SIZE_TYPE__ size_t;
extern "C" size_t strlen (const char*);
}

using namespace std;

int
main ()
{
  if (strlen ("foo") != 3)
    link_error ();
  return 0;
}

