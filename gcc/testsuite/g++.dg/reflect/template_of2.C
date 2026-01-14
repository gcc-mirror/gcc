// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::template_of.

#include <meta>

using namespace std::meta;

struct S {
  template<typename T>
  void fn (T) { }

  template<typename T>
  using U = int;
};

template<typename T>
struct TS {
  template<typename U>
  void fn (T) { }

  template<typename TT>
  using U = int;
};

static_assert (template_of (^^S::fn<int>) == ^^S::fn);
static_assert (template_of (^^TS<int>::fn<int>) == ^^TS<int>::fn);
static_assert (template_of (^^S::U<int>) == ^^S::U);
// FIXME
// template<class T> template<class U> using TS<T>::W = int
//                   template<class U> using TS<int>::W = int
//static_assert (template_of (^^TS<int>::U<int>) == ^^TS<int>::U);
