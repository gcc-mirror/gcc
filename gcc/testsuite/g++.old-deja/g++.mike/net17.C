// { dg-do run  }
// example from the ARM page 292 and 293

extern "C" int printf(const char *, ...);
extern "C" void exit(int);

int i = 0;

class A {
public:
  A() {
    printf("Doing A\n");
    if (++i != 1)
      exit(1);
  }
};

class B {
public:
  B() {
    printf("Doing B\n");
    if (++i != 2)
      exit(1);
  }
};

class C : public virtual A, public virtual B {
public:
  C() {
    printf("Doing C\n");
    if (++i != 3)
      exit(1);
  }
};

class D : public virtual B, public virtual A {
public:
  D() {
    printf("Doing D\n");
    if (++i != 4)
      exit(1);
  }
};

class E : public C, public D {
public:
  E() {
    printf("Doing E\n");
    if (++i != 5)
      exit(1);
  }
} e;


int main() {
  if (++i != 6)
    exit(1);
  return 0;
}
