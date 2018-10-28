// https://issues.dlang.org/show_bug.cgi?id=16980
interface A { void foo(); }
interface B { void bar(); }
interface AB : A, B {}
class C : AB {
    void foo() { assert(false, "Must never be called!"); }
    void bar() {}
}

struct T() {
    AB ab;
    ~this() {
        ab.bar(); // uses wrong vtable
    }
}

T!() tinst; // triggers semantic3 of dtor from Module::semantic(1)

void main()
{
    auto dst = T!()(new C);
}
