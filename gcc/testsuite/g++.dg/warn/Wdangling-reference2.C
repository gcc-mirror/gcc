// PR c++/106393
// { dg-do compile { target c++11 } }
// { dg-options "-Wdangling-reference" }

namespace std {
struct any { void *p; ~any(); };
template <typename _ValueType> _ValueType any_cast(any &&);
template <typename _Tp> struct remove_reference { using type = _Tp; };
template <typename _Tp> _Tp forward(typename remove_reference<_Tp>::type);
template <typename _Tp> typename remove_reference<_Tp>::type move(_Tp);
} // namespace std

const int &r = std::any_cast<int&>(std::any()); // { dg-warning "dangling reference" }

template <class T> struct C {
  T t_; // { dg-warning "dangling reference" }
  C(T);
  template <class U> C(U c) : t_(std::forward<T>(c.t_)) {}
};
struct A {};
struct B {
  B(A);
};
int main() {
  A a;
  C<A> ca(a);
  C<B &&>(std::move(ca));
}
