// PR c++/56973
// { dg-do compile { target c++11 } }

int f()
{
  const int i = 42;
  auto j = *[=]{ return &i; }();
  auto k = []{ return i; }();
  return j+k;
}

int main()
{
  return f() != 84;
}
