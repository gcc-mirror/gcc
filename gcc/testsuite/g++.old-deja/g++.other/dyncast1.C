// { dg-do run  }
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

// 1. downcast

// 1.1. single inheritance case

struct A {virtual ~A(){}};
struct AA : A {};
struct B : A {};
struct BB : B {};
class C : B {};
struct D : C {};

struct CC : B {};
class DD : CC {};

class CCC : protected B {};
class DDD : protected CCC {};

void 
test01 ()
{
  D d;
  if(dynamic_cast<D*> ((A*)&d)) error(1);
  if(dynamic_cast<D*> ((B*)&d)) error(2);
  if(&d != dynamic_cast<D*> ((C*)&d)) error(3); //counter example
  if(dynamic_cast<C*> ((B*)&d)) error(4);
	
  DD dd;
  if(dynamic_cast<DD*> ((A*)&dd)) error(5);
  if(dynamic_cast<DD*> ((B*)&dd)) error(6);

  DDD ddd;
  if(dynamic_cast<DDD*> ((A*)&ddd)) error(7);
  if(dynamic_cast<DDD*> ((B*)&ddd)) error(8);
  if(dynamic_cast<CCC*> ((B*)&ddd)) error(9);
}		

// 1.2. multiple inheritance case
// 1.2.1. all bases are public
 
struct E : D, CC {};
struct EE : CC, D {}; //Will search in reverse order.

void 
test02 ()
{
  E e;
  if(dynamic_cast<E*> ((A*)(D*)&e)) error(10);
  if(dynamic_cast<E*> ((B*)(D*)&e)) error(11);
  if(&e != dynamic_cast<E*> ((C*)(D*)&e)) error(12); //counter example
  if(&e != dynamic_cast<E*> ((B*)(CC*)&e)) error(13); //counter example
  if((CC*)&e != dynamic_cast<CC*> ((B*)(CC*)&e)) error(14); //counter example
  
  EE ee;
  if(dynamic_cast<EE*> ((A*)(D*)&ee)) error(15);
  if(dynamic_cast<EE*> ((B*)(D*)&ee)) error(16);
  if(&ee != dynamic_cast<EE*> ((C*)(D*)&ee)) error(17); //counter example
  if(&ee != dynamic_cast<EE*> ((B*)(CC*)&ee)) error(18); //counter example
  if((CC*)&ee != dynamic_cast<CC*> ((B*)(CC*)&ee)) error(19); //counter example
}		

// 1.2.2 one or more branches are nonpublic

struct X : private BB, E {};
struct Y : AA, private B {};

class XX : BB, E {};

void 
test03 ()
{
  X x;
  if(&x != dynamic_cast<X*>((B*)(CC*)(E*)&x)) error(20); //counter example
  XX xx;
  if(dynamic_cast<XX*>((B*)(CC*)(E*)&xx)) error(21);	
  Y y;
  if(dynamic_cast<Y*>((B*)&y)) error (22);
  if(dynamic_cast<Y*>((A*)(B*)&y)) error (23);
}

// 2. crosscast

struct J {virtual ~J(){};};
struct K : CC, private J {}; 
class KK : J, CC{};
		
void 
test04 ()
{
  E e;
  if(dynamic_cast<CC*> ((B*)(D*)&e)) error(24);
  if((CC*)&e != dynamic_cast<CC*> ((C*)(D*)&e)) error(25); //counter example
  K k;
  if(dynamic_cast<J*> ((B*)&k)) error(26);
  KK kk;
  if(dynamic_cast<J*> ((CC*)&kk)) error(27);
}

int 
main ()
{
  test01();
  test02();
  test03();
  test04();
  return errors ? 1 : 0;
}
