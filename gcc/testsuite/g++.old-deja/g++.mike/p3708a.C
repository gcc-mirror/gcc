// { dg-do run  }
// prms-id: 3708

extern "C" int printf (const char *, ...);
extern "C" int atoi (const char *);

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

class B :  public A1, public virtual A {
public:
  B() { printf ("B is constructed.\n"); }
  virtual void xx(int doit) {
    printf ("B is destructed.\n");
    A1::xx (1);
    if (doit) A::xx (1);
  }
};

int num;

class C : public virtual A, public B {
public:
  C() { ++num; printf ("C is constructed.\n");
      ptr = this;
      }
  virtual void xx(int doit) {
    --num;
    if (ptr != this)
      printf("FAIL\n%x != %x\n", ptr, this);
    printf ("C is destructed.\n");
    B::xx (0);
    if (doit) A::xx (1);
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
  printf ("*** Construct C object!\n");
  C *c = new C();

  int i = 0;

  printf ("*** Try to delete the casting pointer!\n");
  switch (i)
    {
    case 0: fooA1(c);
      break;
    case 1: fooA(c);
      break;
    case 2: fooB(c);
      break;
    case 3: fooC(c);
      break;
    }

  return num!=0;
}
