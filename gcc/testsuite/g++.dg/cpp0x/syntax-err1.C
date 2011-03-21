// PR c++/47198
// { dg-options -std=c++0x }

struct S
{
  template < int > sometype foo (); // { dg-error "sometype. does not name a type" }
  S () = default;
};
