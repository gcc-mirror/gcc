// { dg-do assemble  }
// prms-id: 2573

class X {
public:
  int key();
  virtual int vkey();
  char *add();
};

char *X::add() {
  char *f1 = (char *) &key;	// { dg-error "14:invalid cast" }
  // { dg-error "24:ISO C\\+\\+ forbids taking the address" "" { target *-*-* } .-1 }
  char *f2 = (char *) &vkey;	// { dg-error "14:invalid cast" }
  // { dg-error "24:ISO C\\+\\+ forbids taking the address" "" { target *-*-* } .-1 }
  return f1;
}
