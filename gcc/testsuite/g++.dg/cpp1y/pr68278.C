// { dg-do compile { target c++14 } }

int main()
{
  auto f = []() { return 1; };

  auto q = [=](auto g) {
    using type = decltype(g(f()));
  };
  q([](int x){ return x; });
}
