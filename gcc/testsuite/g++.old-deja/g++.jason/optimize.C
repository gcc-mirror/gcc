// Bug: g++ claims that control can fall off the end of these functions.
// PRMS Id: 4943
// Special g++ Options: -O -pedantic-errors
// Build don't link:

struct A {
  A();
  A(const A&);
  A& operator= (const A&);
  ~A();
};

int f ()
{
  A a[2];
  return 1;
}				// gets bogus error - jump_optimize

int g ()
{
  A a;
  return 1;
}				// gets bogus error - jump_optimize

struct B {
  B();
  B(const B&);
  B& operator= (const B&);
  ~B();
};

inline B::~B()
{
  int i = 2;
  while (i--) ;
}

int h ()
{
  B b;
  return 1;
}				// gets bogus error - jump_optimize
