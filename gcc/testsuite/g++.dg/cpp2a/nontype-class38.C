// PR c++/95369
// { dg-do compile { target c++20 } }

struct S {
  int a;
  int b;
};

struct W {
  int i;
  S s;
};

template <S p>
void fnc()
{
}

template<S s> struct X { };
template<W w> struct Y { };

void f()
{
  fnc<{ .a = 10, .b = 20 }>();
  fnc<{ 10, 20 }>();
  X<{ .a = 1, .b = 2 }> x;
  X<{ 1, 2 }> x2;
  // Brace elision is likely to be allowed.
  Y<{ 1, 2, 3 }> x3;
}
