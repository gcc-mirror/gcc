/* TEST_OUTPUT:
---
fail_compilation/test19473.d(15): Error: union `test19473.P` no size because of forward reference
fail_compilation/test19473.d(31):        error on member `test19473.P.p`
---
 */

// https://issues.dlang.org/show_bug.cgi?id=19473

struct A {
        P p;

        struct UTpl() {
                union {
                        P p;
                }
        }

        alias U = UTpl!();
}

alias B = A.U;

struct C {
        union D {
                B b;
        }
}

union P {
        C.D p;
}
