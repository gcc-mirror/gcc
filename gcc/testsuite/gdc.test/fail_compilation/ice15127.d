/*
TEST_OUTPUT:
---
fail_compilation/ice15127.d(17): Error: basic type expected, not `struct`
fail_compilation/ice15127.d(17): Error: identifier expected for template value parameter
fail_compilation/ice15127.d(17): Error: found `struct` when expecting `)`
fail_compilation/ice15127.d(17): Error: found `ExampleStruct` when expecting `=`
fail_compilation/ice15127.d(17): Error: semicolon expected following auto declaration, not `)`
fail_compilation/ice15127.d(17): Error: declaration expected, not `)`
---
*/

struct ExampleStruct(S) { }

template ExampleTemplate(K)
{
    enum ExampleTemplate(struct ExampleStruct(K)) = K;
}

void main() {}
