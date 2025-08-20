// PR c++/121553
// { dg-do compile { target c++11 } }

void foo () {
 int i;
  auto f = [i] () { int i; return 0; };
}
