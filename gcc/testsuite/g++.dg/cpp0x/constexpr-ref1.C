// PR c++/49172
// { dg-options -std=c++11 }

#define SA(X) static_assert((X),#X)

constexpr int g() { return 42; };
constexpr int(&rg)() = g; // #1

SA(rg() == 42);

constexpr int i = 24;
constexpr int const& ri = i; // #2

SA(&ri == &i);
SA(ri == 24);

void f()
{
  constexpr int(&rg)() = g; // #1

  SA(rg() == 42);

  constexpr static int i = 24;
  constexpr int const& ri = i; // #2

  SA(&ri == &i);
  SA(ri == 24);
}

template <class T>
void f2()
{
  constexpr int(&rg)() = g; // #1

  SA(rg() == 42);

  constexpr static int i = 24;
  constexpr int const& ri = i; // #2

  SA(&ri == &i);
  SA(ri == 24);
}

template void f2<int>();
