// Build don't link:
// Copyright (C) 2000 Free Software Foundation
// Contributed by Nathan Sidwell 22 June 2000 <nathan@codesourcery.com>

#include <stdarg.h>

struct A {
  virtual ~A () {};
};

template <class Type>
void PrintArgs (Type somearg, ...)
{ 
va_list argp;
va_start (argp, somearg); // ERROR - cannot pass non-POD
Type value;
value = va_arg (argp, Type); // ERROR - cannot pass non-POD
va_end (argp);
}

int main (void)
{
A dummy;
PrintArgs (dummy, dummy); // ERROR - cannot pass non-POD
return 0;
}
