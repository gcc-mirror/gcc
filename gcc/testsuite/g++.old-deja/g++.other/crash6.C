// { dg-do assemble  }
// { dg-options "-O2" }

struct E
{
  int f(int);
};

void ha()
{
  enum {X = 0};

  int A, C;

  E vList[10];

  A = (C + 1) % 3;
  vList[1].f(A);
}
