/* TEST_OUTPUT:
---
fail_compilation/fail19038.d(21): Error: cannot implicitly convert expression `a` of type `string[][]` to `const(string)[][]`
fail_compilation/fail19038.d(23): Error: cannot modify `const` expression `c[0]`
---
 * Credit: yshui
 * https://github.com/dlang/dmd/pull/8413#issuecomment-401104961
 * https://issues.dlang.org/show_bug.cgi?id=19038
 */


void test()
{
    /* string[][] is not implicitly converible to const(string)[][],
     * and there is good reason why:
     *
     * https://stackoverflow.com/questions/5055655/double-pointer-const-correctness-warnings-in-c
     */

    string[][] a = [["Lord"]];
    const(string)[][] b = a; // assume this works (and it should not)
    const(string)[] c = ["Sauron"];
    c[0] = "Mordor"; // invalid, because c[0] is const(string)

    b[0] = c; // valid, b[0] is const(string)[]
    // But now, a[0] has become c
    a[0][0] = "Nazgul"; // valid, because a[0][0] is string
    // But this also changes c[0], which shouldn't be possible
}
