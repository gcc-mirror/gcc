// { dg-do run  }
// prms-id: 4667

int counter = 0;
int a = 0;
int b = 0;
int c = 0;

struct A {
  A() { a = counter++; }
};

struct B {
  B() { b = counter++; }
};

struct C : public virtual B {
  C() { c = counter++; }
};

struct D : public virtual A, public virtual C { };

extern "C" int printf(const char*,...);
int main(void) {
  D d;

  if (!(a == 0 && b == 1 && c == 2)) {
    return 1;
  }

  return 0;
}
