// DR 2369
// { dg-do compile { target c++20 } }

template <class T> struct Z {
  typedef typename T::x xx;
};
template <class T> concept C = requires { typename T::A; }; // { dg-message "T::A" }
template <C T> typename Z<T>::xx f(void *, T); // { dg-message "not satisfied" }

struct A {} a;
struct ZZ {
  template <class T, class = typename Z<T>::xx> operator T *();
  operator int();
};
int main() {
  ZZ zz;
  f(1, a); // { dg-error "no match" } no conversion from int to void*
  // { dg-message "cannot convert" "" { target *-*-* } .-1 }
  f(zz, 42); // { dg-error "no match" } C<int> is not satisfied
}
