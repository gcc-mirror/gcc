// Build don't link:

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Oct 1999 <nathan@acm.org>

// Make sure we diagnose bad uses of va_arg

#include <stdarg.h>

struct X {int m;};
struct Y : X {int m;};
struct Z;   // ERROR - forward decl
void fn1(va_list args)
{
  int i = va_arg (args, int);
  Y x = va_arg (args, Y);         // WARNING - cannot receive
  Y y = va_arg (args, struct Y);  // WARNING - cannot receive
  int &r = va_arg (args, int &);  // WARNING - cannot receive
  
  Z z1 = va_arg (args, Z);        // ERROR - incomplete
  const Z &z2 = va_arg (args, Z);       // ERROR - incomplete

  va_arg (args, char);    // ERROR - promote
  va_arg (args, int []);  // ERROR - promote
  va_arg (args, int ());  // ERROR - promote
  va_arg (args, bool);    // ERROR - promote
}
