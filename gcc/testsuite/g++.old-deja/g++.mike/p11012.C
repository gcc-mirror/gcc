// Build don't link:
// Special g++ Options: -Wno-pmf-conversions
// prms-id: 11012

class Foo {
public:
  int f(){}
};

void foo() {
  void *p1 = &Foo::f;
  const void *p2 = &Foo::f;
  int (*p3)() = &Foo::f;
}
