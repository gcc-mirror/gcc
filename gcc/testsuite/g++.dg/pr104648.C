// { dg-do compile }
// { dg-options "-fvtable-verify=preinit" }

struct A {};
struct B : virtual A
{
  B () {};
  B () {}; /* { dg-error "cannot be overloaded with" } */
};
