// PR c++/31617
// Segfault in integer_zerop
// Origin: Andrew Pinski  <andrew_pinski@playstation.sony.com>
// { dg-do compile }
// { dg-options "-O2" }

struct polynomial {
  ~polynomial ();
};

void spline_rep1 ()
{
  new polynomial[0];
}


