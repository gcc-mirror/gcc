// { dg-do assemble  }
// Testcase for all uses of explicit global scope.

int a, B;

struct A { };
int operator+(A&, int);

struct B {
  struct C {
    static int a;
    static int f () { A a; return ::operator+ (a, ::a); } // { dg-bogus "" } 
  };
};

int B::C::a = 0;

struct D : public ::B::C { };	// { dg-bogus "" } 

void f ()
{
  int B;
  ::B::C b;

  B = ::B::C::a;		// { dg-bogus "" } 
  B = ::B::C::f();		// { dg-bogus "" } 
}
