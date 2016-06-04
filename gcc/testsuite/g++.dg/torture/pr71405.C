// PR tree-optimization/71405
// { dg-do compile }

struct C
{
  C () {}
  int i;
};

void *
operator new (__SIZE_TYPE__ x, void *y)
{
  return y;
}

int
main ()
{
  int a;
  new (&a) C;
  return a; 
}
