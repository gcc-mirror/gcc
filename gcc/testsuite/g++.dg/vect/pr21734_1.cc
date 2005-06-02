/* { dg-do compile } */

struct A
{
  int a[4];
  int& operator[](int i) { return a[i]; }
};

struct B : public A
{
  int& operator[](int i) { return A::operator[](i); }
};

void foo(B &b)
{
  for (int i=0; i<4; ++i)
    b[i] = 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
