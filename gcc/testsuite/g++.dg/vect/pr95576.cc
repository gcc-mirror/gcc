// { dg-do compile }
// { dg-additional-options "-O3 -fno-tree-forwprop -fcompare-debug" }

struct S {
  virtual ~S();
  struct S *s;
  virtual void m();
  int f;
  void *d;
};

struct T : S {
  void m();
};

S::~S() {
  if (s) {
    s->f = 0;
    s->d = __null;
  }
}

void T::m() {}
