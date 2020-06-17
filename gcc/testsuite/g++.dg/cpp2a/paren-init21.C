// PR c++/92947 - Paren init of aggregates in unevaluated context.
// { dg-do compile { target c++20 } }

struct A {
  int a;
  int b;
};

int main()
{
  static_assert(__is_constructible(A, int, int));
  decltype(A(1,2)) foo;
  bool b = noexcept(A(1,2));
}
