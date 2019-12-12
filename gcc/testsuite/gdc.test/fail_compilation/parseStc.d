/*
TEST_OUTPUT:
---
fail_compilation/parseStc.d(11): Error: found `;` when expecting `)`
fail_compilation/parseStc.d(11): Error: found `)` when expecting `;` following statement
fail_compilation/parseStc.d(12): Error: redundant attribute `const`
---
*/
void test1()
{
    if (x; 1) {}
    if (const const auto x = 1) {}
}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc.d(25): Error: redundant attribute `const`
fail_compilation/parseStc.d(26): Error: redundant attribute `const`
fail_compilation/parseStc.d(27): Error: conflicting attribute `immutable`
---
*/
void test2()
{
    const const x = 1;
    foreach (const const x; [1,2,3]) {}
    foreach (const immutable x; [1,2,3]) {}
}

/*
TEST_OUTPUT:
---
fail_compilation/parseStc.d(37): Error: redundant attribute `const`
fail_compilation/parseStc.d(38): Error: redundant attribute `const`
---
*/
struct S3 { const const test3() {} }
void test4(const const int x) {}
