// { dg-do run  }
// { dg-options "-O2" }
// Testcase for order of destruction.

extern "C" int printf (const char *, ...);
int c;
int r;

struct B {
  B();
  B( B const& );
  ~B();
};

struct A {
  A();
  A( A const& );
  ~A();
  operator B ();
};

inline A::operator B () { printf( "operator B ()\n"); return B(); }

A f();
void g( B const& );

int
main()
{
  g( f() );
  return r;
}

B::B() { printf( "B::B()\n" ); if (++c != 2) r = 1; }
B::B( B const& ) { printf( "B::B( B const& )\n" ); r = 1; }
B::~B() { printf( "B::~B()\n" ); if (--c != 1) r = 1; }

A::A() { printf( "A::A()\n" ); if (++c != 1) r = 1; }
A::A( A const& ) { printf( "A::A( A const& )\n" ); r = 1; }
A::~A() { printf( "A::~A()\n" ); if (--c != 0) r = 1; }

A f() { printf( "f()\n"); return A(); }
void g( B const& ) { printf( "g()\n"); }
