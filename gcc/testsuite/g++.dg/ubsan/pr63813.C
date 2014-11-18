// PR sanitizer/63813
// { dg-do compile }
// { dg-options "-fsanitize=undefined -O1" }

struct A {};
struct B { long foo () const; A &bar () const; };

A &
B::bar () const
{
  return *reinterpret_cast <A *> (foo ());
}
