// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

struct undeclared<int> { }; // { dg-error "not a class template" }

int
main ()
{
  int foo ();
  int a, b = 10;
  a = foo<; // { dg-error "invalid template-argument-list|invalid" }
  a = foo < b; // { dg-error "invalid template-argument-list|invalid" }
  a = foo<b>; // { dg-error "after template-argument-list|invalid" }
  a = foo<b>(; // { dg-error "expected" }
  a = foo<b>(1; // { dg-error "expected" }
  a = foo<b>(1); // { dg-error "no matching" }
}
