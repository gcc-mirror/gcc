// PR c++/56268
// { dg-do compile { target c++11 } }

template <class T>
struct A {
  A(const A&) noexcept (T::value);
};

struct B {
  static const bool value = true;
};

template <class T>
struct C {
  static const bool value = __has_nothrow_copy (T);
};

#define SA(X) static_assert((X),#X)
SA(C<A<B>>::value);
