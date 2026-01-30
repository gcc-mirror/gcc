/*
TEST_OUTPUT:
---
fail_compilation/fix20075.d(15): Error: none of the overloads of `this` can construct an immutable object with argument types `(int*)`. Expected `immutable(int*)`
fail_compilation/fix20075.d(11):        Candidate is: `fix20075.Foo.this(immutable(int*) a) immutable`
---
*/

struct Foo {
    @disable this();
    immutable this(immutable int* a) {}
}

immutable(Foo) getFoo(int* a) {
    return immutable Foo(a);
}

void main() {
    int x;
    auto foo = getFoo(&x);
}
