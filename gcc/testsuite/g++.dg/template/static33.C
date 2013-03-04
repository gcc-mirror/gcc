// PR c++/52688
// { dg-do link }

template<typename T>
T f()
{
  static const double staticLocalVariable = 100.0;
  struct local
  {
      static double f() { return staticLocalVariable; }
  };
  return T(local::f());
}

int main()
{
  f<double>();
}
