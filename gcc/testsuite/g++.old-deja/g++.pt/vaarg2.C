// Build don't link:
// Copyright (C) 2000 Free Software Foundation
// Contributed by Nathan Sidwell 22 June 2000 <nathan@codesourcery.com>

// Origin GNATS bug report 63 from Kurt Garloff <garloff@tue.nl>
// We attempted to expand va_arg prematurely in a template function.

#include <stdarg.h>

template <class Type>
void PrintArgs (Type somearg, ...)
{ 
va_list argp;
va_start (argp, somearg);
Type value;
while ( ( value = va_arg (argp, Type) ) > 0.0)
  continue;
va_end (argp);
}

int main (void)
{
double dummy = 0;
PrintArgs (dummy, 1.0, 2.0, 3.0, -1.0);
return 0;
}
