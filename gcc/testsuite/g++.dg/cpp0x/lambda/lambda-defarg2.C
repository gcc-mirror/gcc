// PR c++/43886
// { dg-options -std=c++0x }

void f2() {
  int i = 1;
  void g5(int = ([]{ return sizeof i; })());
}
