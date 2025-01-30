// { dg-additional-options "-fno-tree-sra" } */

struct T{
  long f[2];
};
void f1(int);
void g(T&);
void f1()
{
  T a;
  a.~T(); // To force the clobber
  a.f[1] = 1;
  T b = a;
  g(b);
}
