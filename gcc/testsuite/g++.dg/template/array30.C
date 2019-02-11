template <int I>
struct A
{
  int ar[I][I][I][I][I][I][I][I][I][I]; // { dg-error "exceeds maximum object size" }
};

A<66000> a;
