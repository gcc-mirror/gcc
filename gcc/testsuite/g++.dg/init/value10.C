// PR c++/51331
// { dg-do run }

struct A {
  A(): x(10) {}
  virtual ~A() {}

  int x;
};

struct B: public virtual A {
};

struct C: public virtual A {
};

struct D: public B, virtual public C {
  D(): B(), C() {}  // note an explicit call to C() which is auto-generated
};

int main() {
  D* d = new D();

  // Crashes here with the following message:
  // *** glibc detected *** ./test: free(): invalid next size (fast)
  delete d;
}
