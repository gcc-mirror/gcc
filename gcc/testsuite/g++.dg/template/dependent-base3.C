// PR c++/85060
// { dg-do compile { target c++14 } }

struct CA {
  constexpr int foo() const { return 42; }
};

template <class T>
struct CB : CA { };

template <class T>
struct CC : CB<T> {
  constexpr int bar() const {
    const int m = CA::foo();
    return m;
  }

  constexpr int baz() const {
    const T m = CA::foo();
    return m;
  }
};

constexpr CC<double> c;

static_assert( c.bar() == 42, "" );
