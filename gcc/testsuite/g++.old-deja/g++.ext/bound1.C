// Testcase for cast of bound pointer to member function.
// Special g++ Options: -Wno-pmf-conversions
// Build don't link:

struct A {
  int f ();
};

typedef int (*fptr)(A *);
typedef void* vptr;
typedef int (A::*pmf)();

int foo (A* ap, pmf fp, int A::* ip)
{
  fptr p;
  vptr q;
  A a;

  p = (fptr)(ap->*fp);
  p = (fptr)(ap->*fp);
  p = (fptr)(ap->*(&A::f));
  p = (fptr)(a.*fp);
  p = (fptr)(a.*(&A::f));

  q = (vptr)(ap->*fp);
  q = (vptr)(ap->*(&A::f));
  q = (vptr)(a.*fp);
  q = (vptr)(a.*(&A::f));
}
