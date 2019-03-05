// PR c++/85032
// { dg-do compile { target c++17 } }

struct A
{
  constexpr operator bool () { return true; }
  int i;
};

A a;

template <class T>
void f()
{
  constexpr bool b = a;
  static_assert (a);
}

int main()
{
  f<int>();
}
