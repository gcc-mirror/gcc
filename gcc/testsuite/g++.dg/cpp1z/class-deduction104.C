// PR c++/82632
// { dg-do compile { target c++17 } }

template<class T> struct Optional {
  template<class U> Optional(U&&);
};

template<class A> Optional(A) -> Optional<A>;

Optional opt(1729);
Optional dupe(opt);

using ty1 = decltype(opt);
using ty1 = Optional<int>;

using ty2 = decltype(dupe);
using ty2 = Optional<int>;
