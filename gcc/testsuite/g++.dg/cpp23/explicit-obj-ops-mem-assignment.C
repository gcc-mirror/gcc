// P0847R7
// { dg-do compile { target c++23 } }

// uses of member only operators (assignment)

struct S {
  void operator=(this S&, int) {}
};

void non_dep()
{
  S s{};
  s = 0;
}

template<typename = void>
void dependent()
{
  S s{};
  s = 0;
}

void call()
{
  dependent();
}

