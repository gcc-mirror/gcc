// { dg-do assemble  }
// { dg-options "" }

// Make sure the typedef name is used in name mangling, not some random
// anonymous name

struct foo {
  typedef enum { red } color;
  void bar(color r) {
  }
} f;

#ifdef sparc
void f1() asm("bar__3fooQ23foo3$_0");
void f1() {
}
void f2() asm("_bar__3fooQ23foo3$_0");
void f2() {
}
void f3() asm("__bar__3fooQ23foo3$_0");
void f3() {
}
#endif

main() {
  f.bar(foo::red);
}
