// PR c++/13358: g++ should accept a long long constant sans LL suffix
// if -Wno-long-long is in use.
// { dg-do compile }
// { dg-require-effective-target int32plus }
// { dg-options "-std=c++98 -pedantic-errors" }


void use_longlong ()
{
  unsigned long long x1, x2, x3; // { dg-error "ISO C\\+\\+ 1998 does not support 'long long'" }
  // make sure we error with hex, decimal and octal
  x1 = 0x1b27da572ef3cd86; // { dg-error "integer constant is too large for 'long' type" "long long" { target ilp32 } }
  x2 = 1956772631100509574; // { dg-error "integer constant is too large for 'long' type" "long long" { target ilp32 } }
  x3 = 0154476645345674746606; // { dg-error "integer constant is too large for 'long' type" "long long" { target ilp32 } }
}

void use_longlong2 ()
{
  unsigned long long x1, x2, x3; // { dg-error "ISO C\\+\\+ 1998 does not support 'long long'" }
  // make sure we error with hex, decimal and octal
  x1 = 0x1b27da572ef3cd86LL; // { dg-error "long long" }
  x2 = 1956772631100509574LL; // { dg-error "long long" }
  x3 = 0154476645345674746606LL; // { dg-error "long long" }
}
