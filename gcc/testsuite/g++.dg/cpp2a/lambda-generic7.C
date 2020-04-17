// PR c++/94507 - ICE-on-invalid with lambda template.
// { dg-do compile { target c++2a } }

struct S { };

template<typename T, typename U>
auto foo(T, U)
{
  [] <> () { foo (S{}, S{}); }; // { dg-error "expected" }
}
