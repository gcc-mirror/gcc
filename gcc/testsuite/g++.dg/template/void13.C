// PR c++/30299

struct A
{
  int i;
};

template<void> struct B : A  // { dg-error "not a valid type" }
{
  B() { this->i; }
};
