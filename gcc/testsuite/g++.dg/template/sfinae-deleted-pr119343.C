// { dg-do compile { target c++11 } }
// PR c++/119343 - No SFINAE for deleted explicit specializations

struct true_type { static constexpr bool value = true; };
struct false_type { static constexpr bool value = false; };

struct X {
  static void f()=delete;
  template<int> static void g();
};
template<> void X::g<0>()=delete;
struct Y {
  static void f();
  template<int> static void g();
};

template<class T,class=void>
struct has_f : false_type {};
template<class T>
struct has_f<T,decltype(void(T::f))> : true_type {};

static_assert(!has_f<X>::value, "");
static_assert(has_f<Y>::value, "");

template<class T,class=void>
struct has_g0 : false_type {};
template<class T>
struct has_g0<T,decltype(void(T::template g<0>))> : true_type {};

static_assert(!has_g0<X>::value, "");
static_assert(has_g0<Y>::value, "");
