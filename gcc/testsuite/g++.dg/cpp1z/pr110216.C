// DR 2386
// PR c++/110216
// { dg-do compile { target c++17 } }


namespace std{
  template <typename T> struct tuple_size;
}

struct A {
  int x = 0;
};

template <> struct std::tuple_size <::A> {};

auto [x] = A{};

int
main ()
{
}
