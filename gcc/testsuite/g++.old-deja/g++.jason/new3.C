// { dg-do run  }
// { dg-options "-fcheck-new -pedantic" }
// PRMS Id: 6037

extern "C" void * malloc (__SIZE_TYPE__);

int ena = 0;

struct A {
  int i;
  A () { i = 2; }
  void * operator new (__SIZE_TYPE__ s)
  {
    if (ena)
      return 0;
    return malloc (s);
  }
};

struct B {
  int i;
  B () { i = 2; }
  void * operator new (__SIZE_TYPE__ s) throw()
  {
    if (ena)
      return 0;
    return malloc (s);
  }
};

int main ()
{
  ena = 1;
  A *ap = new A;
  B *bp = new B;
  
  return ap || bp ;
}
