// Build don't link: 

struct A {
  struct B {
    B (int);
  };
  static int foop (B);
  int splat () {
    return foop (B (1));
  }
};
