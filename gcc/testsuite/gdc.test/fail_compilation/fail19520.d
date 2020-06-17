/* https://issues.dlang.org/show_bug.cgi?id=19520
TEST_OUTPUT:
---
fail_compilation/fail19520.d(17): Error: incompatible types for `(Empty) is (Empty)`: cannot use `is` with types
fail_compilation/fail19520.d(17):        while evaluating: `static assert((Empty) is (Empty))`
fail_compilation/fail19520.d(18): Error: incompatible types for `(WithSym) is (WithSym)`: cannot use `is` with types
fail_compilation/fail19520.d(18):        while evaluating: `static assert((WithSym) is (WithSym))`
fail_compilation/fail19520.d(19): Error: incompatible types for `(Empty) is (Empty)`: cannot use `is` with types
fail_compilation/fail19520.d(20): Error: incompatible types for `(WithSym) is (WithSym)`: cannot use `is` with types
---
*/
struct Empty { }
struct WithSym { int i; }

void test()
{
    static assert(Empty is Empty);
    static assert(WithSym is WithSym);
    assert(Empty is Empty);
    assert(WithSym is WithSym);
}
