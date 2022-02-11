/* TEST_OUTPUT:
---
fail_compilation/test19112.d(13): Error: cannot implicitly convert expression `[123, 456]` of type `int[]` to `int[1]`
fail_compilation/test19112.d(15): Error: cannot implicitly convert expression `a` of type `int[]` to `int[1]`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=19112

void main()
{
    int[int[1]] aa;
    int* p = [123, 456] in aa;
    int[] a;
    p = a in aa;
}
