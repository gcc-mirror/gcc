/*
TEST_OUTPUT:
---
fail_compilation/ice19295.d(11): Error: `this` for `gun` needs to be type `S2` not type `S1!(gun)`
fail_compilation/ice19295.d(11):        while evaluating `pragma(msg, &gun)`
fail_compilation/ice19295.d(17): Error: template instance `ice19295.S1!(gun)` error instantiating
---
*/
struct S1(T...) {
    auto fun() {
        pragma(msg, &T[0]);
    }
}

struct S2 {
    void gun() {}
    S1!gun overloaded;
}
