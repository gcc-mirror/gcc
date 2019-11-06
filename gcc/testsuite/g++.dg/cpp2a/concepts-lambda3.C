// { dg-do run { target c++2a } }

template<typename T>
concept C1 = __is_same_as(T, int)
  || __is_same_as(T, long long)
  || __is_same_as(T, char);

template<typename T>
concept IsNotLarge = !__is_same_as(T, long long);

template<typename T>
concept IsNotTiny = !__is_same_as(T, char);

template<IsNotLarge T>
struct Foo
{
  static constexpr auto a = [](auto n) { return n; };
  template<IsNotTiny S>
  auto b()
  {
    return [](auto n) { return n; };
  }
};

using Func = int(*)(int);

int main(int, char**)
{
  auto g = []<C1 T> requires IsNotTiny<T>(T t) -> T
    requires IsNotLarge<decltype(t)> { return t; };
  g(5);
  g.operator()<int>(5.5);

  auto z = []<typename T, int N = 5>(T t) requires (N < 4) { return t; };
  z.operator()<int, 3>(5);

  [](auto t) requires true { return t; }(5);
  [](C1 auto t) { return t; }(5);

  auto a0 = [](IsNotLarge auto a) { return [](auto b){ return b; }; };
  auto a1 = a0(1);
  auto a2 = a1(5LL);

  auto b0 = [](auto a) { return [](IsNotLarge auto b){ return b; }; };
  auto b1 = b0(5LL);
  auto b2 = b1(1);

  Foo<int> foo1;
  foo1.a(5.5);
  foo1.a(1LL);
  foo1.b<int>()(5);
  foo1.b<long long>()(5);

  Foo<double> foo2;
  foo2.a(5.5);
  foo2.a(1LL);
  foo2.b<int>()(5);
  foo2.b<long long>()(5);

  Func m1 = [](auto a) -> int requires true { return a; };

  return 0;
}

