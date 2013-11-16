// PR c++/29143

void f(int);

void g(int,int);
void g(int,int,int);

void
h ()
{
  (&f)(1);
  (&g)(1,2,3);
}
