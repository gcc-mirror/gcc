// { dg-do compile }

struct A
{
  A(int);
};

A::A(int i)
{
  int x[1][i];
  x[0][0] = 0;
}
