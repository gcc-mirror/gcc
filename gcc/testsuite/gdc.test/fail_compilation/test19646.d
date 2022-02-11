/* TEST_OUTPUT:
---
fail_compilation/test19646.d(11): Error: cast from `const(int)*` to `int*` not allowed in safe code
---
https://issues.dlang.org/show_bug.cgi?id=19646
 */

@safe:

const x = 42;
int* y = cast(int*)&x;
