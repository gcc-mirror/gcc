/*
TEST_OUTPUT:
---
fail_compilation/fix19613.d(15): Error: function `fix19613.B.a` cannot override `final` function `fix19613.A.a`
fail_compilation/fix19613.d(15): Error: function `fix19613.B.a` does not override any function
fail_compilation/fix19613.d(15):        Did you mean to override `void fix19613.A.a(string)`?
---
*/

class A {
        final void a(int) {}
        void a(string) {}
}
class B : A {
        override void a(int) {}
}
