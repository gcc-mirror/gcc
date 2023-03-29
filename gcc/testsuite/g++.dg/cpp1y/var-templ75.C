// PR c++/108848
// { dg-do compile { target c++14 } }

template<class T>
struct tag_t {
  template<class Sig>
  static constexpr const Sig* var = nullptr;

  template<class Sig>
  static const Sig* fun();
};

template<class T>
constexpr tag_t<T> tag;

template<class T>
void f() {
  tag<int>.var<void>;   // { dg-bogus "expected 'template' keyword" }
  tag<int>.fun<void>(); // { dg-bogus "expected 'template' keyword" }
};
