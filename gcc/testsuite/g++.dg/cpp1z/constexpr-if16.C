// { dg-do compile { target c++17 } }

struct A
{
  constexpr operator bool () { return true; }
  int i;
};

A a;

template <class T> void f()
{
  constexpr bool b = a;
  if constexpr (a) { }
}

int main()
{
  f<int>();
}
