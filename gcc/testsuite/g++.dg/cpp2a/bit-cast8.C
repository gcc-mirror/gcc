// PR c++/98469
// { dg-do compile { target c++20 } }
// { dg-options "-Wall" }

struct S { int s; };

S
foo ()
{
  return __builtin_bit_cast (S, 0);
}
