// A version of nontype-class52.C where explicit template arguments are
// given in the call to f (which during deduction need to be partially
// substituted into the NTTP object V in f's signature).
// { dg-do compile { target c++20 } }

template<class> struct A { };

template<auto> struct B { };

template<class T, A<T> V> void f(B<V>);

int main() {
  constexpr A<int> a;
  f<int>(B<a>{});
}
