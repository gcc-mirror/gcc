// Build don't link: 

int f1 () {
    struct A { A() { a = 2; } int a; } ;
  A aa;
  return aa.a;
}
int f2 () {
    struct A { A() { a = 2; } int a; } ;
  A ab;
  return ab.a;
}
