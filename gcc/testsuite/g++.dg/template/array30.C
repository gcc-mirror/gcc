template <int I>
struct A
{
  int ar[I][I][I][I][I][I][I][I][I][I]; // { dg-error "too large" }
};

A<66000> a;
