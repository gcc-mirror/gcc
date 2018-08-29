// PR c++/70338
// { dg-do compile { target c++11 } }
// { dg-options "-g" }
// { dg-require-effective-target alloca }

template<typename T>
void
foo (int x)
{
  T a[x];
  auto b = [&]() { for (auto &c: a) c = 0.; };
}

int
main ()
{
  foo<double> (3);
}
