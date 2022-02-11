/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayexp.d(24): Error: cannot use `[]` operator on expression of type `int`
fail_compilation/fail_arrayexp.d(25): Error: cannot use `[]` operator on expression of type `void`
fail_compilation/fail_arrayexp.d(26): Error: static array of `const(int)[]` with multiple lengths not allowed
fail_compilation/fail_arrayexp.d(27): Error: only one index allowed to index `string`
fail_compilation/fail_arrayexp.d(28): Error: no `[]` operator overload for type `U`
fail_compilation/fail_arrayexp.d(29): Error: only one index allowed to index `(int, string)`
---
*/

int i;
string str;
union U {}
alias typeAlias = const(int)[];
void getVoid();
alias getTuple(T...) = T;

void test19534() // https://issues.dlang.org/show_bug.cgi?id=19534
{
    U agg;
#line 24
    auto p = i[0];
    auto q = getVoid()[0];
    alias r = getTuple!(typeAlias[0, 1]);
    auto s = str[0, 1, 2];
    auto t = agg[];
    auto u = getTuple!(int, string)[1, 2];
}
