/* TEST_OUTPUT:
---
fail_compilation\aa_assign.d(11): Error: associative arrays can only be assigned values with immutable keys, not `char[]`
---
*/

void test_mutable_key()
{
    int[char[]] aa;
    char[] str = "123".dup;
    aa[str] = 3;
    assert(str in aa);
}
