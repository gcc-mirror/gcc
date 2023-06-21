// DR 2256
// PR c++/103091
// { dg-do run }

static int i;

struct A {
  ~A() { ++i;}
};

void f() {
  goto L;
  A a;
L:
  return;
}

int main() {
  f();
  if (i != 1)
    __builtin_abort();
}
