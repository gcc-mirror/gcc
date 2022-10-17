/* TEST_OUTPUT:
---
fail_compilation/test20610.d(20): Error: cannot modify `const` expression `field`
---
 */

// https://issues.dlang.org/show_bug.cgi?id=20610

struct S
{
    int what;
}

void main()
{
    S record;

    foreach (const ref field; record.tupleof)
    {
        field = 10;
    }
}
