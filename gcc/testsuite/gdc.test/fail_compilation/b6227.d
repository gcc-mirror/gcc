/* TEST_OUTPUT:
---
fail_compilation/b6227.d(16): Error: comparison between different enumeration types `X` and `Y`; If this behavior is intended consider using `std.conv.asOriginalType`
fail_compilation/b6227.d(16):        while evaluating: `static assert(!(X.O != Y.U))`
fail_compilation/b6227.d(17): Error: comparison between different enumeration types `X` and `Y`; If this behavior is intended consider using `std.conv.asOriginalType`
fail_compilation/b6227.d(17):        while evaluating: `static assert(X.O == Y.U)`
---
*/
enum X {
    O,
    R
}
enum Y {
    U
}
static assert(!(X.O != Y.U));
static assert( (X.O == Y.U));
