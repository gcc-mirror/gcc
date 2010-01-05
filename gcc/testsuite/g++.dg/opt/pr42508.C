// PR tree-optimization/42508
// { dg-do run }
// { dg-options "-O1 -fipa-sra" }

extern "C" void abort ();

int v[10], vidx;

struct A
{
  A *prev;
  int i;
  ~A()
  {
    v[vidx++] = i;
    delete prev;
  }
};

int
main ()
{
  A *a1 = new A ();
  A *a2 = new A ();
  a1->prev = 0;
  a1->i = 1;
  a2->prev = a1;
  a2->i = 2;
  delete a2;
  if (vidx != 2 || v[0] != 2 || v[1] != 1)
    abort ();
  return 0;
}
