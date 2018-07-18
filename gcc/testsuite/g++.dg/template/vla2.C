// PR c++/28879
// { dg-do compile }
// { dg-options "" }
// { dg-require-effective-target alloca }

struct A
{
  static int i;
  int j;
};

template<int> void foo ()
{
  int x[A::i];
//int y[A().j];
}

void bar ()
{
  foo<6> ();
}
