// Build don't link: 

struct A { A() { a = 2; } int a; };

struct B {
  struct A { A() { a = 2; } int a; };
  A aa;
};
char xx[]="../tests/m3.cc:4: redefinition of `struct A'";
