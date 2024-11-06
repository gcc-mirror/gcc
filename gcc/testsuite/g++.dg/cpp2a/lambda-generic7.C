// PR c++/94507 - ICE-on-invalid with lambda template.
// { dg-do compile { target c++20 } }

struct S { };

template<typename T, typename U>
auto foo(T, U)
{
  [] <> () { foo (S{}, S{}); }; // { dg-error "" }
}
