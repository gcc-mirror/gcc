// https://issues.dlang.org/show_bug.cgi?id=23773

/*
TEST_OUTPUT:
---
fail_compilation/fail23773.d(14): Error: assignment cannot be used as a condition, perhaps `==` was meant?
---
*/

void main()
{
    int i;
    int[] arr;
    assert(arr.length = i);
}
