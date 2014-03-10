// PR c++/47198
// { dg-do compile { target c++11 } }

struct S
{
  template < int > sometype foo (); // { dg-error "sometype. does not name a type" }
  S () = default;
};
