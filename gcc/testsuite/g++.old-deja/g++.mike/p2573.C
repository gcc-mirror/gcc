// prms-id: 2573

class X {
public:
  int key();
  virtual int vkey();
  char *add();
};

char *X::add() {
  char *f1 = (char *) &key;	// ERROR - 
  char *f2 = (char *) &vkey;	// ERROR - 
  return f1;
}
