// { dg-do assemble  }
// prms-id: 2573

class X {
public:
  int key();
  virtual int vkey();
  char *add();
};

char *X::add() {
  char *f1 = (char *) &key;	// { dg-error "" } 
  char *f2 = (char *) &vkey;	// { dg-error "" } 
  return f1;
}
