// { dg-do compile }

struct S
{
  S() {}
  S(S&) {}
};

S f()
{
  S s;
  return s;
}

void g()
{
  S s;
  throw s;
}
