// Bug: g++ expanded b->member() multiple times, causing the optimizer to
// decide that things weren't related and optimize 'die' into an infinite
// loop.

struct A {
  virtual ~A() { }
  void f (bool) { }
};

typedef void (A::*pmf_void)();
typedef void (A::*pmf_bool)(bool);

struct B {
  ~B() {}
  pmf_void member() const { return mbr; }
  pmf_void mbr;
};

A *a;
B *b;

void die (bool param) {
  pmf_bool pmf = (pmf_bool)(b->member());
  (a->*pmf)(param);
}

int main ()
{
  A a2;
  B b2;

  b2.mbr = reinterpret_cast<pmf_void>(&A::f);

  a = &a2;
  b = &b2;

  die (true);
}
