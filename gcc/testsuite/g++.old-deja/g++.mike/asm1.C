// Build don't link:
// Special g++ Options: 

struct A {
  static void foo() asm("_my_routine");
};

void A::foo() {
}
