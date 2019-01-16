/* TEST_OUTPUT:
---
compilable/b6227.d(17): Deprecation: Comparison between different enumeration types `X` and `Y`; If this behavior is intended consider using `std.conv.asOriginalType`
compilable/b6227.d(18): Deprecation: Comparison between different enumeration types `X` and `Y`; If this behavior is intended consider using `std.conv.asOriginalType`
---
*/
enum X {
    O,
    R
}
enum Y {
    U
}
static assert( (X.O == cast(const)X.O));
static assert( (X.O == X.O));
static assert( (X.O != X.R));
static assert(!(X.O != Y.U));
static assert( (X.O == Y.U));
