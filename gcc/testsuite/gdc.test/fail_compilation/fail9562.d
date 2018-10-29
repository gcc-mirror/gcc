/*
REQUIRED_ARGS: -o-
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/fail9562.d(17): Error: int[] is not an expression
fail_compilation/fail9562.d(18): Error: no property 'reverse' for type 'int[]'
fail_compilation/fail9562.d(19): Error: no property 'sort' for type 'int[]'
fail_compilation/fail9562.d(20): Error: no property 'dup' for type 'int[]'
fail_compilation/fail9562.d(21): Error: no property 'idup' for type 'int[]'
---
*/

void main()
{
    alias A = int[];
    auto len  = A.length;
    auto rev  = A.reverse;
    auto sort = A.sort;
    auto dup  = A.dup;
    auto idup = A.idup;
}
