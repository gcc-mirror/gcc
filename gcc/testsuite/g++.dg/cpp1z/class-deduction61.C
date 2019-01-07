// PR c++/81486
// { dg-do compile { target c++17 } }

template <typename... Ts>
struct foo
{
  template <typename... Us>
  foo(Us...) { }
};

int
main ()
{
  auto f = foo();
}
