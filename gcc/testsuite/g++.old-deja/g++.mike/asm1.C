// { dg-do assemble  }
// { dg-options "" }

struct A {
  static void foo() asm("_my_routine");
};

void A::foo() {
}
