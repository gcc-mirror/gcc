// { dg-do assemble  }
// PRMS Id: 6056

struct Foo {
  Foo()           { }		// { dg-error "" } candidate
  Foo(int i = 25) { }		// { dg-error "" } candidate
};

int main()
{
  Foo* f1 = new Foo();		// { dg-error "" } ambiguous
}
