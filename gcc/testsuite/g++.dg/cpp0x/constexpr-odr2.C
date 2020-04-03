// PR c++/92062 - ODR-use ignored for static member of class template.
// { dg-do run { target c++11 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

template<int> struct A {
  static const bool x;
  enum { force_instantiation =! &x}; // odr-uses A<...>::x
};

int g;

template<int I>
const bool A<I>::x = (g = 42, false);

void f(A<0>) {}        // A<0> must be complete, so is instantiated
int main()
{
  if (g != 42)
    __builtin_abort ();
}
