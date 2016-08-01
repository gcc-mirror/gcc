// PR c++/72766
// { dg-options "-Wno-vla" }

long fn1() {
  const int a = fn1();
  int b[a];
  int c = *(&b[0] + sizeof(0));
}
