// Author: Alfred Miniarik <a8601248@unet.univie.ac.at>
// test of dynamic_cast
// runtime detecting of nonpublic
// inheritance within a cast
// and therefor failing with result 0.

extern "C" void abort();
extern "C" int printf (const char *, ...);

static int errors = 0;

void error(int i)
{
  printf("Error %i\n",i);
  errors++;
}

struct A {virtual ~A(){}};
struct B : private virtual A {};
struct C : virtual A {};
struct D : B, C {};

int 
main()
{
  D d;
  A* ap= &d;
  if(&d != dynamic_cast<D*>(ap)) error(1);
  if((B*)&d != dynamic_cast<B*>(ap)) error(2);
  if((C*)&d != dynamic_cast<C*>(ap)) error(3);
  return errors ? 1 : 0;
}
