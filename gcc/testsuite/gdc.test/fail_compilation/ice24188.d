/*
REQUIRED_ARGS: fail_compilation/ice24188_a/ice24188_c.d
TEST_OUTPUT:
---
fail_compilation/ice24188.d(9): Error: module `ice24188_c` from file fail_compilation/ice24188_a/ice24188_c.d must be imported with 'import ice24188_c;'
---
*/
auto b() {
    import fail_compilation.ice24188_a.ice24188_c : D;

    struct A {
        D e;
    }
}
