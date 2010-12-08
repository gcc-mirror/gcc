// { dg-do assemble  }
// PRMS Id: 6056

struct Foo {
  Foo()           { }		// { dg-message "note" }
  Foo(int i = 25) { }		// { dg-message "note" }
};

int main()
{
  Foo* f1 = new Foo();		// { dg-error "ambiguous" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 11 }
}
