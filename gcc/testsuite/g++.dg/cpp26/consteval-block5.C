// { dg-do compile { target c++26 } }
// Test consteval blocks, as specified by P2996.

void bar () { }

template<int N>
constexpr void
fn ()
{
  if (N > 0)
    bar ();
}

template<typename>
struct S {
  consteval { fn<1>(); }
};

template<>
struct S<int> {
  consteval { fn<0>(); }
};

S<int> s1;

template<typename T>
struct S<T*> {
  consteval { fn<0>(); }
};

S<int *> s2;

template<typename T, int N>
struct W {
  consteval { T t; fn<N - 1>(); }
};

template<typename T>
struct W<T, 0> {
  consteval { T t; fn<0>(); }
};

template<>
struct W<char, 0> {
  consteval { fn<0>(); }
};

W<int, 0> w1;
W<int, 1> w2;
W<char, 0> w3;

template<typename>
void
f ()
{
  consteval { fn<1>(); }
}

template<>
void
f<int> ()
{
  consteval { fn<0>(); }
}

void
g ()
{
  f<int> ();
}
