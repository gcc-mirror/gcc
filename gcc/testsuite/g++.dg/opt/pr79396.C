// PR middle-end/79396
// { dg-do compile }
// { dg-options "-fnon-call-exceptions -O2" }
// { dg-additional-options "-mfma" { target i?86-*-* x86_64-*-* } }

struct A { A (); ~A (); };

float
foo (float x)
{
  A a;
  return __builtin_pow (x, 2) + 2;
}
