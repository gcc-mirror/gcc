// { dg-do compile }
// { dg-options "" }

struct B
{
  int a;
  B() : a(({ 1; })) {}
};
