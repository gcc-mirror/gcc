// { dg-do run  }
// Test that conversion from D* to B*& works properly.

extern "C" int printf (const char *, ...);

struct V {
  int a;
};

struct B: virtual V {
  int b;
};

struct D: B {
  int c;
};

V* gp = 0;

void foo(V * const &r) {
  gp = r;
}

int bar(V *r) {
  return (r != gp);
}

int main() {
  D *p = new D;
  foo(p);
  return bar(p);
}
