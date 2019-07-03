struct A {};

typedef struct
{
  void foo() {}
} A::B;  // { dg-error "3:typedef" }
