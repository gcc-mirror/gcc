// { dg-do compile { target c++11 } }

struct A
{
  ~A();
};

template<class T>
struct W {
  T t;
  template<class U>
  constexpr W(U&& u) : t(u) {}
};

template <class T>
constexpr W<T> make_w(T& w) { return W<T>(w); }

A a;
constexpr auto w = make_w(a);	// { dg-error "" }
