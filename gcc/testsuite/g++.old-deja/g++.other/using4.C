// Build don't link:

// Based on a testcase by Martin Bachtold <martinb@coyotesystems.com>

struct foo {
  void m();
};

struct bar : foo {
  using foo::m;
  void m(int);
};

void f() {
  bar b;
  b.m();
  b.m(1);
}
