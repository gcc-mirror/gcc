// { dg-do run }
// { dg-options "-O2" }

struct D { int x; };
struct W
{
  W () {}
  D & operator * () { return d; }
  D d;
};

int
foo (int y)
{
  W m;
  (*m).x = (y > 1 ? y : 0);
  return (*m).x;
}

int
main ()
{
  return (foo (6) != 6);
}
