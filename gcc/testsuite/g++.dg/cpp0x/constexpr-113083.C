// PR c++/113083
// { dg-do compile { target c++11 } }
// { dg-options "-Os" }

struct A { constexpr A (); };

void
foo ()
{
  A b;
}

constexpr
A::A ()
{
}
