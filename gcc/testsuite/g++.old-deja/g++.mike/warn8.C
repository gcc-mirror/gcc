// Build don't link:

struct foo {
  bool test();
};
bool func();

void test() {
  foo A;
  bool (foo::* pmf)() = &foo::test;
  bool (*pf)() = func;

  if (A.test) ;			// WARNING - 
  if (func) ;			// WARNING - 
  if (bool(A.test)) ;		// WARNING - 
  if (bool(func)) ;
  if (pmf) ;
  if (pf) ;
}
