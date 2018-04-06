struct A {}; 

struct B : virtual A
{
  B () {};
  B () {};  // { dg-error "cannot be overloaded" }
};
