/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayexp.d(24): Error: cannot use `[]` operator on expression of type `int`
fail_compilation/fail_arrayexp.d(25): Error: cannot use `[]` operator on expression of type `void`
fail_compilation/fail_arrayexp.d(26): Error: static array of `const(int)[]` with multiple lengths not allowed
fail_compilation/fail_arrayexp.d(27): Error: only one index allowed to index `string`
fail_compilation/fail_arrayexp.d(28): Error: no `[]` operator overload for type `U`
fail_compilation/fail_arrayexp.d(16):        perhaps define `auto opIndex() {}` for `fail_arrayexp.U`
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

/*
TEST_OUTPUT:
---
fail_compilation/fail_arrayexp.d(49): Error: no `[3.."4"]` operator overload for type `S`
fail_compilation/fail_arrayexp.d(42):        perhaps define `auto opSlice(int lower, string upper) {}` for `fail_arrayexp.S`
fail_compilation/fail_arrayexp.d(50): Error: no `[]` operator overload for type `S`
fail_compilation/fail_arrayexp.d(42):        perhaps define `auto opIndex(int, string, char) {}` for `fail_arrayexp.S`
---
*/

struct S
{
}

void testSlice()
{
    S s;
    const b = s[3 .. "4"];
    const c = s[3, "4", 'c'];
}
