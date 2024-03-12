/*
TEST_OUTPUT:
---
fail_compilation/ice9439.d(12): Error: calling non-static function `foo` requires an instance of type `Derived`
fail_compilation/ice9439.d(12):        while evaluating: `static assert(foo())`
fail_compilation/ice9439.d(19): Error: template instance `ice9439.Base.boo!(foo)` error instantiating
---
*/

class Base {
    void boo(alias F)() {
        static assert(F());
    }
}

class Derived : Base {
    int foo() { return 1; }
    void bug() {
        boo!(foo)();
    }
}
