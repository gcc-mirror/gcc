// { dg-do assemble  }

struct foo {
  bool test();
};
bool func();

void test() {
  foo A;
  bool (foo::* pmf)() = &foo::test;
  bool (*pf)() = func;

  if (A.test) ;			// { dg-error "" } 
  if (func) ;			// { dg-warning "" } 
  if (bool(A.test)) ;		// { dg-error "" } 
  if (bool(func)) ;             // { dg-warning "" } 
  if (pmf) ;
  if (pf) ;
}
