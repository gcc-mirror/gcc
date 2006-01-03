// PR c++/25638

struct A { ~A(); }; // { dg-error "candidate" }  

struct B : A
{
  template<int> friend A::~A(); // { dg-error "match" }
};
