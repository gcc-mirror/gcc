// { dg-do compile { target c++11 } }

void
foo (int n)
{
  auto a = new int [n] [[]];
  auto b = new int [n] [[]] [42] [[]] [1] [[]];
  delete[] b;
  delete[] a;
}
