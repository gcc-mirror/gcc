// PR c++/51326

struct A
{
  virtual int& foo(); // { dg-message "overridden" }
};

struct B : A
{
  B& foo();           // { dg-error "conflicting return type" }
};
