/*
TEST_OUTPUT:
---
fail_compilation/fail6611.d(11): Error: cannot post-increment array slice `x[]`, use pre-increment instead
---
*/

void main()
{
    auto x = new int[](10);
    x[]++;
}
