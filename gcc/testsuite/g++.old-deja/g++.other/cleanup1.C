// { dg-do run  }
// Bug: fold is too eager about pushing down CLEANUP_POINT_EXPR.

int d;

struct A {
  A() { }
  ~A() { d = 1; }
};

int f (const A& a)
{
  return 1;
}

int main ()
{
  if (f (A()) && d == 0)
    return 0;
  return 1;
}
