// Build don't link: 

class A {
  int a;
 public:
  A (int aa = 3);
};

class B {
  class A {
  public:
    A (int, int);
  };
  A aa;
 public:
  B (int);
};

extern void foo();
B::B (int z) : aa (1, z) {
  foo ();
}
