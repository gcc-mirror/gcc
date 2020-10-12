// DR 2369
// { dg-do compile { target c++20 } }

template <class T> struct Z {
  typedef typename T::x xx;
};
template <class T> concept C = requires { typename T::A; };
template <C T> typename Z<T>::xx f(void *, T); // #1
template <class T> void f(int, T); // #2
struct A {} a;
struct ZZ {
  template <class T, class = typename Z<T>::xx> operator T *();
  operator int();
};
int main() {
  ZZ zz;
  f(1, a); // OK, deduction fails for #1 because there is no conversion from int to void*
  f(zz, 42); // OK, deduction fails for #1 because C<int> is not satisfied
}
