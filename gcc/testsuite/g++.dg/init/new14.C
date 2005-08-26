// PR c++/23491

struct X
{
  int m;
};

void f(int n)
{
  const X *p = new const X[1] () ;
}
