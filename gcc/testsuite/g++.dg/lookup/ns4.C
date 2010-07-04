// PR c++/43145
// { dg-do link }

namespace N {
  void f();
}

void N::f()
{
  extern int i;
  extern void g();
  i = 1;
  g();
}

namespace N {
  int i;
  void g() { }
}

int main() { }
