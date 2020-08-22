// { dg-do compile { target c++20 } }

template<typename T>
concept False = false;

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
    return [](False auto n) { return n; };
  }
};

template<IsNotTiny T>
struct Bar
{
  static constexpr auto a =
    []<IsNotTiny R>(R t) { return t; }('a'); // { dg-error "no match" }
  char b =
    []<IsNotTiny R>(R t) { return t; }('b'); // { dg-error "no match" }

  static constexpr auto a2 =
    [](char t) requires false { return t; }('a'); // { dg-error "no match" }
  char b2 =
    [](char t) requires false { return t; }('b'); // { dg-error "no match" }
};

template<IsNotLarge S>
S c =
  []<IsNotTiny R>(R t) { return t; }('c'); // { dg-error "no match" }
template<IsNotLarge S>
S c2 =
  [](char t) requires false { return t; }('c'); // { dg-error "no match" }

Bar<long long> bar;

void test0()
{
  auto bar_a = bar.a;
  auto bar_b = bar.b;
  auto c = ::c<char>;
  auto bar_a2 = bar.a2;
  auto bar_b2 = bar.b2;
  auto c2 = ::c2<char>;

  auto g0 = []<False T>(T t) { return t; };
  auto g1 = []<typename T> requires False<T> (T t) { return t; };
  auto g2 = []<typename T>(T t) requires False<decltype(t)> { return t; };
  auto g3 = [](int t) requires False<decltype(t)> { return t; }; // { dg-error "non-templated" }
  auto g4 = [](False auto t) { return t; };
  auto g5 = [](auto t) requires False<decltype(t)> { return t; };
  auto g6 = [](int t) requires False<int> { return t; }; // { dg-error "non-templated" }
  auto g7 = [](int t) requires false { return t; };	 // { dg-error "non-templated" }
  g0(0); // { dg-error "no match" }
  g1(0); // { dg-error "no match" }
  g2(0); // { dg-error "no match" }
  g3(0);
  g4(0); // { dg-error "no match" }
  g5(0); // { dg-error "no match" }
  g6(0);
  g7(0);
}

void test1()
{
  int var{-1};
  auto g0 = [&]<False T>(T t) { return t; };
  auto g1 = [&]<typename T> requires False<T> (T t) { return t; };
  auto g2 = [&]<typename T>(T t) requires False<decltype(t)> { return t; };
  auto g3 = [&](int t) requires False<decltype(t)> { return t; }; // { dg-error "non-templated" }
  auto g4 = [&](False auto t) { return t; };
  auto g5 = [&](auto t) requires False<decltype(t)> { return t; };
  auto g6 = [&](int t) requires False<int> { return t; }; // { dg-error "non-templated" }
  auto g7 = [&](int t) requires false { return t; };	  // { dg-error "non-templated" }
  g0(0); // { dg-error "no match" }
  g1(0); // { dg-error "no match" }
  g2(0); // { dg-error "no match" }
  g3(0);
  g4(0); // { dg-error "no match" }
  g5(0); // { dg-error "no match" }
  g6(0);
  g7(0);
}

void test2()
{
  auto x = []<IsNotTiny T>(auto a, T t, auto b)
    requires IsNotTiny<decltype(a)> && IsNotLarge<decltype(b)>
    { return a + t + (T)b; };
  x(5LL, 2LL, 1);

  x('0', 2LL, 1LL); // { dg-error "no match" }
  x(5LL, '0', 1LL); // { dg-error "no match" }
  x(5LL, 2LL, 1LL); // { dg-error "no match" }
}

void test3()
{
  auto x = []<IsNotTiny T>(IsNotTiny auto a, T t, IsNotLarge auto b)
    { return a + t + (T)b; };
  x(5LL, 2LL, 1);

  x('0', 2LL, 1LL); // { dg-error "no match" }
  x(5LL, '0', 1LL); // { dg-error "no match" }
  x(5LL, 2LL, 1LL); // { dg-error "no match" }
}

void test4()
{
  auto g = []<C1 T> requires IsNotTiny<T>(T t) -> T
    requires IsNotLarge<decltype(t)> { return t; };
  g(5.5); // { dg-error "no match" }
  g('a'); // { dg-error "no match" }
  g(1LL); // { dg-error "no match" }
}

void test5()
{
  Foo<int> foo1;
  foo1.a(5.5);
  foo1.a(1LL);
  foo1.b<char>(); // { dg-error "no match" }
  foo1.b<long long>()(5); // { dg-error "no match" }

  Foo<double> foo2;
  foo2.a(5.5);
  foo2.a(1LL);
  foo2.b<char>(); // { dg-error "no match" }
  foo2.b<long long>()(5); // { dg-error "no match" }
}

using Func = int(*)(int);

void test6()
{
  Func f1 = [](int a) requires false { return a; }; // { dg-error "non-templated" }
  Func f2 = [](auto a) requires false { return a; }; // { dg-error "cannot convert" }
}

