// PR c++/53788
// { dg-do compile { target c++11 } }

struct t { static const bool value = true; };
struct f { static const bool value = false; };

template<typename T>
struct has_static {
  template<typename X>
  static t check(X*, decltype(T::fun())* = 0); // { dg-error "without object" }
  static f check(...);

  typedef decltype(check((T*)(0))) ret;
  static const bool value = ret::value;
};

struct test { int fun() { return 0; } };

bool b = has_static<test>::value;
