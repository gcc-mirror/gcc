// { dg-do compile { target c++26 } }
// Test consteval blocks, as specified by P2996.

void die () {}

template<int N>
constexpr void
fn ()
{
  if (N > 0)
    die ();
}

template<int N>
void
fn2 ()
{
  struct S {
    consteval {
      fn<N>();
    }
  };
}

template<int N>
struct A {
  struct B {
    consteval {
      fn<N>();
    }
  };
  template<int M>
  struct C {
    consteval {
      fn<N + M>();
    }
  };
};

template<int N>
struct D {
  constexpr static int i = 0;
  struct E {
    consteval {
      fn<i>();
    }
  };
};

A<0>::B b;
A<0>::C<0> c;
D<0>::E e;

void
f ()
{
  fn2<0>();
}

static constexpr int j = 0;
const int x = 0;

consteval {
  fn<j>();
  consteval {
    fn<j + j>();
    consteval {
      fn<j + j + j>();
      consteval {
	fn<j + j + x>();
	consteval {
	  fn<j + x>();
	}
      }
    }
  }
}

struct R { constexpr R() {} };

template<int N>
constexpr auto X = N;

consteval {
  R{};
  constexpr auto x = 0;
  fn<x>();
  fn<X<0>>();
  if consteval
    {
      fn<j>();
    }
  else
    {
      die ();
    }
}

template<typename T>
struct G {
  consteval {
    using U = T[3];
    U arr{};
    int i = arr[2];
  }
};

G<int> g;
