// Testcase for all uses of explicit global scope.
// Build don't link:

int a, B;

struct A { };
int operator+(A&, int);

struct B {
  struct C {
    static int a;
    static int f () { A a; return ::operator+ (a, ::a); } // gets bogus error
  };
};

int B::C::a = 0;

struct D : public ::B::C { };	// gets bogus error

void f ()
{
  int B;
  ::B::C b;

  B = ::B::C::a;		// gets bogus error
  B = ::B::C::f();		// gets bogus error
}
