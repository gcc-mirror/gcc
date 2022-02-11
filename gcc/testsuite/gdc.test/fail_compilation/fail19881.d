/* REQUIRED_ARGS: -preview=dip1000
 * TEST_OUTPUT:
---
fail_compilation/fail19881.d(12): Error: address of local variable `local` assigned to return scope `input`
---
 */

// https://issues.dlang.org/show_bug.cgi?id=19881

@safe int* test(return scope int* input) {
    int local = 42;
    input = &local;

    return input;
}
