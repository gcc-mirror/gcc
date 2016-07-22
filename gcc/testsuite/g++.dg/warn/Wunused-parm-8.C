// { dg-do compile { target c++14 } }
// { dg-options "-Wunused-but-set-parameter" }

auto l = [](auto t) -> decltype(true ? t : 0) { return {}; };

int main()
{
  l(42);
}
