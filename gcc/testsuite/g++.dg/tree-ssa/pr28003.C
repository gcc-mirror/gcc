// PR tree-optimization/28003
// Alias grouping needs to be computed after call clobbering, because it
// changes around the tags and variables in a way that makes our
// call clobbering computation incorrect.
// { dg-do run }
// { dg-options "-O2" }
extern "C" void abort(void);
struct A
{
  int i, j[9];
  A() : i(1) { j[0]=j[1]=j[2]=j[3]=j[4]=j[5]=j[6]=j[7]=j[8]=0; }
};

struct B
{
  A a;
};

B b[] =
{
  {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
  {}, {}, {}, {}, {}, {}, {}, {}, {}, {},
  {}, {}, {}, {}, {}
};

int main()
{
  if (1 - b[sizeof(b)/sizeof(B) - 1].a.i != 0)
    abort();
  return 0;
}
