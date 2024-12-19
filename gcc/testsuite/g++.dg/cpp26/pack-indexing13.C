// PR c++/117937
// { dg-do compile { target c++26 } }

using size_t = decltype(sizeof(0));

template<size_t...>
struct seq {};

void g(auto...) {}

void
f (auto... args)
{
  [&]<size_t... i>(seq<i...>) {
      g(args...[i]...);
  }(seq<0>());
}

int
main ()
{
  f(0);
}
