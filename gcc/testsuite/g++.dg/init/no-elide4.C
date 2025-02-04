// PR c++/114619
// { dg-do "compile" { target c++11 } }
// { dg-options "-fno-elide-constructors" }

struct X {
  X(const X&) {}
};
extern X x;
void foo () {
  new X[1]{x};
}
