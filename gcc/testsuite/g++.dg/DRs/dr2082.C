// DR 2082

void f() {
  int i;
  extern void h(int x = sizeof(i));
}

class A {
  void f(A* p = this) { } // { dg-error "this" }
};

int h(int a, int b = sizeof(a));
