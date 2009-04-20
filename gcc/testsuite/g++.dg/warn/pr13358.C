// PR c++/13358: g++ should accept a long long constant sans LL suffix
// if -Wno-long-long is in use.
// { dg-do compile }
// { dg-require-effective-target int32plus } 
// { dg-options "-std=c++98 -Wno-long-long -pedantic-errors" }


void use_longlong ()
{
  unsigned long long x1, x2, x3; 
  // make sure it's ok with hex, decimal and octal
  x1 = 0x1b27da572ef3cd86;
  x2 = 1956772631100509574;
  x3 = 0154476645345674746606;
}

void use_longlong2 ()
{
  unsigned long long x1, x2, x3; 
  // make sure it's ok with hex, decimal and octal
  x1 = 0x1b27da572ef3cd86LL;
  x2 = 1956772631100509574LL;
  x3 = 0154476645345674746606LL;
}
