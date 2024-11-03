// { dg-do link }
// { dg-options "-std=c++20 -fcontracts " }

int foo (int x)
{
  auto f1 = [] (int y)
    [[pre: y > 0]] { return y * y; };
  return f1 (x);
}

int main ()
{
  return foo (-2);
}
