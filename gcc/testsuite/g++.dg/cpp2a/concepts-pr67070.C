// { dg-do compile { target c++20 } }

template <class T>
concept C1 =
  requires { typename T::type; } && T::type::value;

template <class T>
concept C2 =
  requires {
    typename T::Type;
    requires T::Type::value;
  };

template <class T>
  requires (!C1<T>)
void f1() { }

template <class T>
  requires (!C2<T>)
void f2() { }

struct S { };

void test()
{
  f1<S>();
  f2<S>();
}

// ------------------


template<typename T>
concept C = requires (T t) { t.f(); };

template<typename A, typename B>
  requires (!(C<A> && C<B>))
void g1() { }

template<typename A, typename B>
  requires (!C<A> || !C<B>)
void g2() { }

struct X {
  void f();
};

void test2() {
  g1<X, X>(); // { dg-error "" }
  g2<X, X>(); // { dg-error "" }
}