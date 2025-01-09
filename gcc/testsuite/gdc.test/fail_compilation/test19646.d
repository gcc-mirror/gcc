/* TEST_OUTPUT:
---
fail_compilation/test19646.d(12): Error: cast from `const(int)*` to `int*` can't initialize `@safe` variable `y`
fail_compilation/test19646.d(12):        Source type is incompatible with target type containing a pointer
fail_compilation/test19646.d(18): Error: `@safe` variable `z` cannot be initialized by calling `@system` function `f`
---
https://issues.dlang.org/show_bug.cgi?id=19646
 */

@safe:
const x = 42;
int* y = cast(int*)&x;

@system:

@system int* f() { return cast(int*) 0xDEADBEEF; };

@safe int* z = f();
