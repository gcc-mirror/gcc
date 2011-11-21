// PR c++/49229
// { dg-options -std=c++0x }

extern void* enabler;

template<bool, class = void>
struct enable_if {};

template<class T>
struct enable_if<true, T> {
  typedef T type;
};

template<class... Bn>
struct and_;

template<class B1>
struct and_<B1> : B1 {};

template<class, class>
struct is_same {
  static constexpr bool value = false;
};

template<class T>
struct is_same<T, T> {
  static constexpr bool value = true;
};

template<class... T>
struct S {
  template<class... U,
    typename enable_if<and_<is_same<T, U>...>::value>::type*& = enabler // { dg-error "no type" }
  >
  S(U...){}
};

S<bool> s(0);			// { dg-error "no match" }
