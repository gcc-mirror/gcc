// PR c++/104294
// { dg-do compile { target c++17 } }

template<class> struct B;

template<class R, class... Args>
struct B<R(Args...)> {
  template<class T> struct C { C(T); };
  C(decltype(nullptr)) -> C<void*>;
};

using ty1 = decltype(B<char(int)>::C{0});
using ty1 = B<char(int)>::C<int>;

using ty2 = decltype(B<char(int)>::C{nullptr});
using ty2 = B<char(int)>::C<void*>;
