// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation
// Contributed by Nathan Sidwell 22 June 2000 <nathan@codesourcery.com>

#include <stdarg.h>

struct A {
  virtual ~A () {}
};

template <class Type>
void PrintArgs (Type somearg, ...)
{ 
va_list argp;
va_start (argp, somearg);
Type value;
value = va_arg (argp, Type); // { dg-warning "non-POD" } cannot pass non-POD
va_end (argp);
}

int main (void)
{
A dummy;
PrintArgs (dummy, dummy); // { dg-warning "non-POD" } cannot pass non-POD
// { dg-message "instantiated" "inst" { target *-*-* } 24 }
return 0;
}
