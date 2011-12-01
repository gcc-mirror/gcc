// PR c++/51326

struct A
{
  virtual int& foo(); // { dg-error "overriding" }
};

struct B : A
{
  B& foo();           // { dg-error "conflicting return type" }
};
