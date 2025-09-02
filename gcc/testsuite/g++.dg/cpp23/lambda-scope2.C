// P2036R3 - Change scope of lambda trailing-return-type
// PR c++/102610
// { dg-do compile { target c++23 } }
// From LLVM's test/SemaCXX/lambda-capture-type-deduction.cpp

template <typename T, typename U>
constexpr bool is_same = false;

template <typename T>
constexpr bool is_same<T, T> = true;

void
f ()
{
  int y;

  static_assert(is_same<const int &,
			decltype([x = 1] -> decltype((x)) { return x; }())>);

  static_assert(is_same<int &,
                        decltype([x = 1] mutable -> decltype((x)) { return x; }())>);

  static_assert(is_same<const int &,
                        decltype([=] -> decltype((y)) { return y; }())>);

  static_assert(is_same<int &,
                        decltype([=] mutable -> decltype((y)) { return y; }())>);

  // Clang++ rejects this one, though the only difference is the extra (),
  // and without the () the result is correct, as demonstrated above.
  static_assert(is_same<const int &,
                        decltype([=]() -> decltype((y)) { return y; }())>);

  static_assert(is_same<int &,
                        decltype([=]() mutable -> decltype((y)) { return y; }())>);

  static_assert(is_same<const int &,
			decltype([y] -> decltype((y)) { return y; }())>);

  static_assert(is_same<int &,
                        decltype([y] mutable -> decltype((y)) { return y; }())>);


  auto ref = [&x = y](
                 decltype([&](decltype(x)) { return 0; }) y) {
    return x;
  };
}

void
nested ()
{
  int x, y, z;
  (void)[&](
      decltype([&](
                   decltype([=](
                                decltype([&](
                                             decltype([&](decltype(x)) {})) {})) {})) {})){};

  (void)[&](
      decltype([&](
                   decltype([&](
                                decltype([&](
                                             decltype([&](decltype(y)) {})) {})) {})) {})){};

  (void)[=](
      decltype([=](
                   decltype([=](
                                decltype([=](
                                             decltype([&]<decltype(z)> {})) {})) {})) {})){};
}

void
test_noexcept ()
{
  int y;

  static_assert(noexcept([x = 1] noexcept(is_same<const int &, decltype((x))>) {}()));
  static_assert(noexcept([x = 1] mutable noexcept(is_same<int &, decltype((x))>) {}()));
  static_assert(noexcept([y] noexcept(is_same<const int &, decltype((y))>) {}()));
  static_assert(noexcept([y] mutable noexcept(is_same<int &, decltype((y))>) {}()));
  static_assert(noexcept([=] noexcept(is_same<const int &, decltype((y))>) {}()));
  static_assert(noexcept([=] mutable noexcept(is_same<int &, decltype((y))>) {}()));
  static_assert(noexcept([&] noexcept(is_same<int &, decltype((y))>) {}()));
  static_assert(noexcept([&] mutable noexcept(is_same<int &, decltype((y))>) {}()));
}

void
check_params ()
{
  int i = 0;
  int &j = i;

  [=](decltype((j)) jp, decltype((i)) ip) {
    static_assert(is_same<const int&, decltype((j))>);
    static_assert(is_same<const int &, decltype((i))>);
    static_assert(is_same<int &, decltype((jp))>);
    static_assert(is_same<int &, decltype((ip))>);
  };

  [=](decltype((j)) jp, decltype((i)) ip) mutable {
    static_assert(is_same<int &, decltype((j))>);
    static_assert(is_same<int &, decltype((i))>);
    static_assert(is_same<int &, decltype((jp))>);
    static_assert(is_same<int &, decltype((ip))>);
    static_assert(is_same<int &, decltype(jp)>);
    static_assert(is_same<int &, decltype(ip)>);
  };

  [a = 0](decltype((a)) ap) mutable {
    static_assert(is_same<int &, decltype((a))>);
    static_assert(is_same<int, decltype(a)>);
    static_assert(is_same<int &, decltype(ap)>);
    decltype(a) x;
    decltype((a)) y = x;
    static_assert(is_same<int &, decltype(y)>);
  };

  [a = 0](decltype((a)) ap) {
    static_assert(is_same<const int &, decltype((a))>);
    static_assert(is_same<int, decltype(a)>);
    static_assert(is_same<int&, decltype((ap))>);
    decltype(a) x;
    decltype((a)) y = x;
    static_assert(is_same<const int &, decltype(y)>);
  };

  int a;
  [a](decltype((a)) ap) mutable {
    static_assert(is_same<int &, decltype((a))>);
    static_assert(is_same<int, decltype(a)>);
    static_assert(is_same<int &, decltype(ap)>);
    decltype(a) x;
    decltype((a)) y = x;
    static_assert(is_same<int &, decltype(y)>);
  };

  [a](decltype((a)) ap) {
    static_assert(is_same<const int &, decltype((a))>);
    static_assert(is_same<int, decltype(a)>);
    static_assert(is_same<int&, decltype((ap))>);
    decltype(a) x;
    decltype((a)) y = x;
    static_assert(is_same<const int &, decltype(y)>);
  };
}

template <typename T>
void
check_params_tpl ()
{
  T i = 0;
  T &j = i;
  (void)[=](decltype((j)) jp, decltype((i)) ip) {
    static_assert(is_same<const int&, decltype((j))>);
    static_assert(is_same<const int &, decltype((i))>);
    // In these two, clang++ produces 'const int&'.  Why, when it's
    // the same as in check_params, just not a template?
    static_assert(is_same<int &, decltype((jp))>);
    static_assert(is_same<int &, decltype((ip))>);
  };

  (void)[=](decltype((j)) jp, decltype((i)) ip) mutable {
    static_assert(is_same<int &, decltype((j))>);
    static_assert(is_same<int &, decltype((i))>);
    static_assert(is_same<int &, decltype((jp))>);
    static_assert(is_same<int &, decltype((ip))>);
    static_assert(is_same<int &, decltype(jp)>);
    static_assert(is_same<int &, decltype(ip)>);
  };

  (void)[a = 0](decltype((a)) ap) mutable {
    static_assert(is_same<int &, decltype((a))>);
    static_assert(is_same<int, decltype(a)>);
    static_assert(is_same<int &, decltype(ap)>);
  };
  (void)[a = 0](decltype((a)) ap) {
    static_assert(is_same<const int &, decltype((a))>);
    static_assert(is_same<int, decltype(a)>);
    static_assert(is_same<int&, decltype((ap))>);
  };
}

template<typename T>
void
test_requires ()
{
  int x;

  [x = 1]() requires is_same<const int &, decltype((x))> {} ();
  [x = 1]() mutable requires is_same<int &, decltype((x))> {}
  ();
  [x]() requires is_same<const int &, decltype((x))> {} ();
  [x]() mutable requires is_same<int &, decltype((x))> {}
  ();
  [=]() requires is_same<const int &, decltype((x))> {} ();
  [=]() mutable requires is_same<int &, decltype((x))> {}
  ();
  [&]() requires is_same<int &, decltype((x))> {}
  ();
  [&]() mutable requires is_same<int &, decltype((x))> {}
  ();
  [&x]() requires is_same<int &, decltype((x))> {}
  ();
  [&x]() mutable requires is_same<int &, decltype((x))> {}
  ();

  [x = 1]() requires is_same<const int &, decltype((x))> {} ();
  [x = 1]() mutable requires is_same<int &, decltype((x))> {} ();
}

void
use ()
{
  test_requires<int>();
  check_params_tpl<int>();
}
