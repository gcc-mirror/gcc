// P0847R7
// { dg-do compile { target c++23 } }

// uses of member only operators (subscript)

// execution paths for subscript with 1 argument and 0 and 2+ arguments are different
// therefore we should additionally test the 0 and 2 argument cases as well

struct S {
  void operator[](this S&) {}
  void operator[](this S&, int) {}
  void operator[](this S&, int, int) {}
  template<typename... Args>
  void operator[](this S&, Args... args) {}
};

void non_dep()
{
  S s{};
  s[];
  s[0];
  s[0, 0];
  s[0, 0, 0];
}

template<typename = void>
void dependent()
{
  S s{};
  s[];
  s[0];
  s[0, 0];
  s[0, 0, 0];
}

void call()
{
  dependent();
}

