// Build don't link: 

struct A { A() { a = 2; } int a; };

int f1 () {
  struct A { A() { a = 2; } int a; };
  A aa;
  return aa.a;
}
