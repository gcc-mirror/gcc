// Test mangling of type casts
// { dg-options "-fabi-version=2 -fabi-compat-version=2 -Wabi=0" }
// { dg-do compile }

template<int i> class A {};
template<bool b> class B {};

template<int i> void f(A<i> &, B<bool(i)> &) {}
template<int i> void g(A<i> &, B<static_cast<bool>(i)> &) {} // { dg-warning "mangle" }

int main()
{
  A<1> a;
  B<true> b;
  f(a, b);
  g(a, b);
}

// { dg-final { scan-assembler "\n_?_Z1fILi1EEvR1AIXT_EER1BIXcvbT_EE\[: \t\n\]" } }
// { dg-final { scan-assembler "\n_?_Z1gILi1EEvR1AIXT_EER1BIXcvbT_EE\[: \t\n\]" } }
