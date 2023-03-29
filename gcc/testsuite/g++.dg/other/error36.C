// PR c++/106983
// { dg-do compile { target c++20 } }

typedef unsigned long long A;
typedef union
{
  struct B s; // { dg-error "incomplete" }
  A a;
} U;
void f (A x, unsigned int b)
{
  const U y = {.a = x};
}
