// Contributed by Cary Coutant <ccoutant@google.com>
// Origin: PR debug/41063
// { dg-do compile }

struct A {
  virtual void run();
};

void test() {
  struct B : public A {
    void run() {
      struct C : public A {
	C() { }
	B *b_;
      };
      C c;
    }
  };
  B b;
}
