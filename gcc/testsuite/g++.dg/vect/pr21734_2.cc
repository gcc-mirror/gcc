/* { dg-do compile } */

struct A
{
  int a[4];
  int* operator[](int i) { return &a[i]; }
};

void foo(A a1, A &a2)
{
  a1[1][1]=0;
  for (int i=0; i<4; ++i)
    a2.a[i]=0;
}

