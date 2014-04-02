// PR c++/43886
// { dg-do compile { target c++11 } }

void f2() {
  int i = 1;
  void g5(int = ([]{ return sizeof i; })());
}
