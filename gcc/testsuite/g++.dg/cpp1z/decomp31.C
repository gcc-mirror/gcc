// PR c++/81888
// { dg-do compile { target c++17 } }

struct S {
  bool s = true;
};

auto [a] = S{};

template <class T>
bool
foo () noexcept
{
  auto [c] = T{};
  return c;
}

const bool b = foo<S> ();
