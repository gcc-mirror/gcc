// Test that built-in functions are recognized with a prototype.
// Origin: Roger Sayle  Mar 20, 2002
// Copyright (C) 2002 Free Software Foundation.
//
// Special g++ Options: -O2

typedef __SIZE_TYPE__ size_t;
extern "C" size_t strlen (const char*);
extern "C" void link_error (void);

int
main ()
{
  if (strlen ("foo") != 3)
    link_error ();
  return 0;
}

