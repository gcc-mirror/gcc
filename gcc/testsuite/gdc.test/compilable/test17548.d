// REQUIRED_ARGS: -c

module test17548;

struct S1 {
    void foo(scope S2 arg) {}
    int myField;
}

enum cnst = 4321;

import imports.fwdref2_test17548;
