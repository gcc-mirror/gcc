// { dg-do run  }
// { dg-options "" }
// prms-id: 6004

class A {
public:
  static int foo() asm("_my_routine");
};

int bar1() asm("foo__1A");
int bar2() asm("_foo__1A");
int bar3() asm("__foo__1A");
int bar1() { return 45; }
int bar2() { return 44; }
int bar3() { return 43; }

int A::foo() { return 42; }

int
main() {
  return A::foo() - 42;
}
