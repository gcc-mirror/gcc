// { dg-do run  }
// prms-id: 3708

extern "C" int printf (const char *, ...);
extern "C" void exit(int);

void *ptr;

class A {
public:
  A() { printf ("A is constructed.\n"); }
  virtual void xx(int doit) { printf ("A is destructed.\n"); }
};

class A1 {
public:
  A1() { printf ("A1 is constructed.\n"); }
  virtual void xx(int doit) { printf ("A1 is destructed.\n"); }
};

class B : public virtual A, public A1 {
public:
  B() { printf ("B is constructed.\n"); }
  virtual void xx(int doit) {
    printf ("B is destructed.\n");
    A1::xx (1);
    if (doit) A::xx (1);
  }
};

int num;

class C : public virtual A {
public:
  C() { printf ("C is constructed.\n");
      }
  virtual void xx(int doit) {
    printf ("C is destructed.\n");
    if (doit) A::xx (1);
  }
};

class D : public C, public B {
public:
  D() { ++num; printf ("D is constructed.\n");
      ptr = this;
      }
  virtual void xx(int doit) {
    --num;
    if (ptr != this) {
      printf("FAIL\n%x != %x\n", ptr, this);
      exit(1);
    }
    printf ("D is destructed.\n");
    C::xx (0);
    B::xx (0);
  }
};

void fooA(A *a) {
  printf ("Casting to A!\n");
  a->xx (1);
}
void fooA1(A1 *a) {
  printf ("Casting to A1!\n");
  a->xx (1);
}

void fooB(B *b) {
  printf ("Casting to B!\n");
  b->xx (1);
}

void fooC(C *c) {
  printf ("Casting to C!\n");
  c->xx (1);
}

int main(int argc, char *argv[]) {
  printf ("*** Construct D object!\n");
  D *d = new D();

  printf ("*** Try to delete the casting pointer!\n");
  fooA1(d);
  return num!=0;
}
