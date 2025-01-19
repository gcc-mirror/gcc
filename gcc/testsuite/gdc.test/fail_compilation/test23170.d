/*
TEST_OUTPUT:
---
fail_compilation/test23170.d(10): Error: this array literal causes a GC allocation in `@nogc` delegate `__lambda_L10_C15`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=23170

@nogc:
enum lambda = () => badAlias([1, 2, 3]);
alias badAlias = (int[] array) => id(array);
int[] id(int[] array) { return array; }
