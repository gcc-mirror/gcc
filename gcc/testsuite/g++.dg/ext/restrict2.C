// PR c++/60872
// { dg-options "" }

typedef double *__restrict T;
void f(T* p)
{
  void *p2 = p;
}
