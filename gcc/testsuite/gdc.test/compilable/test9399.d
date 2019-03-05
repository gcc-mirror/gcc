// REQUIRED_ARGS: -c -inline -Icompilable/imports
// EXTRA_SOURCES: imports/test9399a.d

import imports.test9399a;
void fun(int a) {
    void nested() {
        a = 42;
    }
    call!nested();
}
