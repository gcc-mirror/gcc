// Build don't link:

struct foo {
  bool test();
};
bool func();

void test() {
  foo A;
  bool (foo::* pmf)() = &foo::test;
  bool (*pf)() = func;

  if (A.test) ;			// ERROR - 
  if (func) ;			// WARNING - 
  if (bool(A.test)) ;		// ERROR - 
  if (bool(func)) ;             // WARNING - 
  if (pmf) ;
  if (pf) ;
}
