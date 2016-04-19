// { dg-do compile { target c++11 } }
// { dg-options "-Wunused-but-set-parameter" }

template <typename... Ts> void sink(Ts...);

struct A { int i; };

template <int... I>
void f(A a)
{
  return sink((a.i + I)...);
}

int main()
{
  f<>(A());
}
