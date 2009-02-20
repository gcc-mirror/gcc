// { dg-do assemble  }
// { dg-options "-Wno-abi" { target arm_eabi } }

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Oct 1999 <nathan@acm.org>

// Make sure we diagnose bad uses of va_arg

#include <stdarg.h>

struct X {int m;};
struct Y : X {int m;};
struct Z;   // { dg-error "forward decl" } 
void fn1(va_list args)
{
  int i = va_arg (args, int);
  Y x = va_arg (args, Y);         // { dg-warning "cannot receive" } 
  Y y = va_arg (args, struct Y);  // { dg-warning "cannot receive" } 
  int &r = va_arg (args, int &);  // { dg-warning "cannot receive" } 
  
  Z z1 = va_arg (args, Z);        // { dg-error "incomplete" } 
  const Z &z2 = va_arg (args, Z);       // { dg-error "incomplete" } 

  va_arg (args, char);    // { dg-warning "promote" } 
  // { dg-message "should pass" "pass" { target *-*-* } 24 }
  // { dg-message "abort" "abort" { target *-*-* } 24 }
  va_arg (args, int []);  // { dg-error "array with unspecified bounds" } promote
  va_arg (args, int ());  // { dg-warning "non-POD" } promote
  va_arg (args, bool);    // { dg-warning "promote" "promote" } 
  // { dg-message "abort" "abort" { target *-*-* } 29 }
}
