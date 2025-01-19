/* REQUIRED_ARGS: -preview=dip1000
 * TEST_OUTPUT:
---
fail_compilation/fail19881.d(13): Error: assigning address of local variable `local` to return scope `input` is not allowed in a `@safe` function
fail_compilation/fail19881.d(13): Error: assigning address of variable `local` to `input` with longer lifetime is not allowed in a `@safe` function
---
 */

// https://issues.dlang.org/show_bug.cgi?id=19881

@safe int* test(return scope int* input) {
    int local = 42;
    input = &local;

    return input;
}
