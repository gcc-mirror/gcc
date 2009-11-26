// PR c++/42026, DR 239
// The local extern declaration prevents arg-dependent lookup.
// { dg-do link }

namespace NS {
  class T { };
  void g(T, int);
}
NS::T parm;
void g(NS::T, float) { }
int main() {
  extern void g(NS::T, float);
  g(parm, 1);
}
