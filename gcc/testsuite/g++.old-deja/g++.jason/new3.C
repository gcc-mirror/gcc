// PRMS Id: 6037
// Special g++ Options: -fcheck-new

extern "C" void * malloc (__SIZE_TYPE__);

struct A {
  int i;
  A () { i = 2; }
};

int ena = 0;
void * operator new (__SIZE_TYPE__ s)
{
  if (ena)
    return 0;
  return malloc (s);
}

main ()
{
  ena = 1;
  A* ap = new A;
}
