// PR c++/88538
// { dg-do compile { target c++2a } }

struct S {
  unsigned a;
  unsigned b;
  constexpr S(unsigned _a, unsigned _b) noexcept: a{_a}, b{_b} { }
};

template <S p>
void fnc()
{
}

template<S s> struct X { };

void f()
{
  fnc<{10,20}>();
  X<{1, 2}> x;
}
