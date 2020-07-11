typedef int int32_t __attribute__((mode (__SI__)));

template <int32_t I>
struct A
{
  int32_t ar[I][I][I][I][I][I][I][I][I][I]; // { dg-error "exceeds maximum object size" }
};

A<66000> a;
