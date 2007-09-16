// PR c++/31617
// Segfault in integer_zerop
// Origin: Andrew Pinski  <andrew_pinski@playstation.sony.com>
// { dg-do compile }

struct polynomial {
  ~polynomial ();
};

void spline_rep1 ()
{
  new polynomial[0];  // { dg-bogus "allocating zero-element array" }
}


