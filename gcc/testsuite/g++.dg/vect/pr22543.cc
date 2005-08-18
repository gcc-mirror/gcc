/* { dg-do compile } */

struct A
{
  int i, j;

  A() : i(), j() {}
  ~A() {}

  operator int() { return 0; }
};

struct B
{
  A foo() const { return A(); }
};

struct X { ~X(); };

struct C
{
  C();

  int z[4];
};

C::C()
{
  for (int i=0; i<4; ++i)
    z[i]=0;

  X x;

  for (int i=0; i<4; ++i)
    int j = B().foo();
}

/* { dg-final { cleanup-tree-dump "vect" } } */
