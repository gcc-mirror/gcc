// PR c++/57404
// { dg-do compile { target c++11 } }
// { dg-options "-g" }
// { dg-require-effective-target alloca }

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
