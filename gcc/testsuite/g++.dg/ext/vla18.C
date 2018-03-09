// PR c++/84767
// { dg-do compile }
// { dg-options "" }

int v[1][10];

struct A
{
  A (int);
};

A::A (int i)
{
  typedef int T[1][i];
  T *x = (T *) v;
  (*x)[0][0] = 0;
}

A a = 10;
