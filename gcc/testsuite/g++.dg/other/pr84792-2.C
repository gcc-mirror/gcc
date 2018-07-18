struct A {};

typedef struct
{
  void foo() {}
} A::B;  // { dg-error "typedef" }
