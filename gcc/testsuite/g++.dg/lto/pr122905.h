struct A {
  typedef char *B;
  struct C { C (B x) {} };
  C c;
  enum { D = 15 };
  union { char e[16]; };
  A (const char *x) : c {e} {}
};
A foo ();
