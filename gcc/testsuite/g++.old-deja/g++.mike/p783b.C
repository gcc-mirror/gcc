// This one check for objects being destroyed twice.  The bug it is
// looking for is the extra dtor call on C() even though it is never
// built.
// prms-id: 783

extern "C" int printf (const char *, ...);
extern "C" void exit (int);

class C {
  int i;
public:
//  C() {printf ("C ctor at %x\n", this);}
//  ~C() {printf ("C dtor at %x\n", this);}
  C() {
    i = 1;
  }
  ~C() {
    if (i != 1) {
      exit(1);
    }
    i = 0;
  }
};

C g;

C func()  {
  return g;
}

int main(int argc, char**argv) {
  C c,d;
//  printf ("\n");
  c = (argc != 1) ? C() : d;
//  printf ("\n");
  return 0;
}
