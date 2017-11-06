// { dg-do run  }
// { dg-options "-fcheck-new" }

struct A {
  A(): i(42) { }
  A(int j): i(j) { }
  int i;
};

A* ap = new A (1);
A* ap2 = new A[3];

int
main ()
{
  if (ap->i != 1 || ap2[0].i != 42 || ap2[1].i != 42 || ap2[2].i != 42)
    return 1;

  A* ap = new A (1);
  A* ap2 = new A[3];

  if (ap->i != 1 || ap2[0].i != 42 || ap2[1].i != 42 || ap2[2].i != 42)
    return 1;

  return 0;
}
