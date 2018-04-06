struct A {};

typedef struct
{
  virtual void foo() {}
} A::B;  // { dg-error "typedef" }
