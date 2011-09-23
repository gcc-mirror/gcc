// PR c++/20039
// { dg-do compile }

struct M
{
  M() : m(0) { }
  int m;
};

struct X
{
  M m;
  int i;
};

int mymain()
{
  const X *p = new const X[2]; // { dg-error "uninitialized const" }
  return 0;
}
