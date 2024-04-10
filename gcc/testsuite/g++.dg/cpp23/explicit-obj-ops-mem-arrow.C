// P0847R7
// { dg-do compile { target c++23 } }

// uses of member only operators (arrow)

struct S {
  int _v;
  S* operator->(this S& self) { return &self; }
};

void non_dep()
{
  S s{};
  (void)s->_v;
}

template<typename = void>
void dependent()
{
  S s{};
  (void)s->_v;
}

void call()
{
  dependent();
}

