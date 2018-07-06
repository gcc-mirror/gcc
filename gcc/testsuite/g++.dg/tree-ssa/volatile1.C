// PR c++/84151
// { dg-additional-options "-fdump-tree-gimple" }
// { dg-final { scan-tree-dump-not {\*this} "gimple" } }

struct A {
  static int& bar(int& a) {
    return a;
  }
  static int i;

  int foo() volatile {
    int v = c;
    return i + bar(v);
  }

  int c;
};

int A::i = 0;

A a;

int main() {
  a.c = 2;
  a.foo();

  return 0;
}
