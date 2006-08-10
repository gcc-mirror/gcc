/* { dg-do compile } */

void g(const void*);
struct B
{
  int* x[2];
  int *p;
  B()
  {
     for (int** p=x; p<x+4; ++p)
      *p = 0;
  }
  ~B()
   {
      g(p);
   }
};
void bar()
{
  const B &b = B();
  g(&b);
}

/* { dg-final { cleanup-tree-dump "vect" } } */
