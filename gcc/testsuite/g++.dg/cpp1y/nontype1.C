// PR c++/91129 - wrong error with binary op in template argument.
// { dg-do compile { target c++14 } }

template<class T, T v>
struct C
{
  constexpr operator T() const { return v; }
  constexpr auto operator()() const { return v; }
};

template<class T, int N>
struct A
{
};

template<int N>
void foo ()
{
  A<int, C<int, 6>{}> a0;
  A<int, !C<int, 6>{}> a1;
  A<int, N / C<int, 6>{}> a2;
  A<int, N % C<int, 6>{}> a3;
  A<int, N * C<int, 6>{}> a4;
  A<int, N ^ C<int, 6>{}> a5;
  A<int, N | C<int, 6>{}> a6;
  A<int, N & C<int, 6>{}> a7;
  A<int, N + C<int, 6>{}> a8;
  A<int, N - C<int, 6>{}> a9;
  A<int, -C<int, 6>{}> a10;
  A<int, (N >> C<int, 6>{})> a11;
  A<int, N << C<int, 6>{}> a12;
  A<int, ~C<int, 6>{}> a13;
  A<int, N || C<int, 6>{}> a14;
  A<int, N && C<int, 6>{}> a15;
  A<int, N == C<int, 6>{}> a16;
  A<int, N != C<int, 6>{}> a17;
}

int main()
{
  foo<10>();
}
