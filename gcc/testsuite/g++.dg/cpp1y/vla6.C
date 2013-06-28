// PR c++/57404
// { dg-options "-std=c++11 -g" }

void f (int i)
{
  int a[i];
  [&a] {};
}

void g (int i)
{
  int a[i];
  [&a] {} ();
}
