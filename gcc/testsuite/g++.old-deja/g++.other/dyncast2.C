// { dg-do run  }
// Author: Alfred Miniarik <a8601248@unet.univie.ac.at>
// test of dynamic_cast
// runtime detecting of valid 
// downcasts within nonpublic 
// baseclasses.

extern "C" void abort();
extern "C" int printf (const char *, ...);

static int errors = 0;

void error(int i)
{
  printf("Error %i\n",i);
  errors++;
}

// 1. downcast
// 1.1 single inheritance case

struct A {virtual ~A(){} int i;};
struct B : A {int i;};
struct C : B {int i;};
struct CC : C {};
class D : C {int i;};

struct E : D {int i;};
class F : E {int i;};

void 
test01 ()
{
  D d;
  if((C*)&d != dynamic_cast<C*> ((A*)&d)) error(1);
  if((C*)&d != dynamic_cast<C*> ((B*)&d)) error(2);
  if((B*)&d != dynamic_cast<B*> ((A*)&d)) error(3);

  E e;
  if((C*)&e != dynamic_cast<C*> ((A*)&e)) error(4);

  F f;
  if((C*)&f != dynamic_cast<C*> ((B*)&f)) error(5);
  if((B*)&f != dynamic_cast<B*> ((A*)&f)) error(6);
  if((E*)&f != dynamic_cast<E*> ((D*)&f)) error(7);
  if(dynamic_cast<E*> ((C*)&f)) error(8); //counter example
}		

// 1.2 multiple inheritance case

struct G : CC, F{};
		
void 
test02 ()
{
  G g;
  if((B*)(F*)&g != dynamic_cast<B*> ((A*)(F*)&g)) error(9);
  if(dynamic_cast<D*> ((A*)(F*)&g)) error(10);
  if(dynamic_cast<G*> ((B*)(F*)&g)) error(11);
}

// 2. crosscast (always fail)

struct I : C{};
struct J : F{};
struct K : I, J{};
class L : K{};
		
void 
test03 ()
{
  L l;
  if(dynamic_cast<J*> ((I*)&l)) error(12);
  if(dynamic_cast<J*> ((E*)&l)) error(13);
  if(dynamic_cast<I*> ((J*)&l)) error(14);
}

int 
main ()
{
  test01();
  test02();
  test03();
  return errors ? 1 : 0;
}
