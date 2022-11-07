// { dg-do compile }

struct S
{
  S() {}
  S(S&) {}
};

S f()
{
  S s;
  return s; // { dg-error "cannot bind non-const lvalue reference" "" { target c++23 } }
}

void g()
{
  S s;
  throw s; // { dg-error "cannot bind non-const lvalue reference" "" { target c++23 } }
}
