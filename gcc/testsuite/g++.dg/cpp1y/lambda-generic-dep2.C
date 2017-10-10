// { dg-do compile { target c++14 } }

struct A { void operator()(int) const {} };

template <class T>
void f()
{
  constexpr A a {};

  [=](auto b) {
    a(b);
  }(42);
}

int main()
{
  f<int>();
}
