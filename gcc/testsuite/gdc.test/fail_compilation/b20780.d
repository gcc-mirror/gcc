/*
TEST_OUTPUT:
---
fail_compilation/b20780.d(10): Error: `@identifier` or `@(ArgumentList)` expected, not `@)`
fail_compilation/b20780.d(11): Error: `@identifier` or `@(ArgumentList)` expected, not `@,`
fail_compilation/b20780.d(11): Error: basic type expected, not `,`
---
*/

void f(@){}
void g(@,){}
