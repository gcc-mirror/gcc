/* TEST_OUTPUT:
TRANSFORM_OUTPUT: remove_lines(called from here)
---
fail_compilation\aaerrors.d-mixin-29(29): Error: `assert(aai[1] == 0)` failed
fail_compilation\aaerrors.d-mixin-30(30): Error: `assert((aai[1] = 1) == 0)` failed
fail_compilation\aaerrors.d-mixin-31(31): Error: `assert(*(1 in aai) == 3)` failed
fail_compilation\aaerrors.d-mixin-32(32): Error: `assert(aai.remove(2))` failed
fail_compilation\aaerrors.d-mixin-33(33): Error: `assert(aai != [1:2])` failed
fail_compilation\aaerrors.d-mixin-34(34): Error: `assert(aai == [1:3])` failed
fail_compilation\aaerrors.d-mixin-41(41): Error: `assert(aas[1].x == 0)` failed
fail_compilation\aaerrors.d-mixin-42(42): Error: `assert((aas[1] = 1).x == 0)` failed
fail_compilation\aaerrors.d-mixin-43(43): Error: `assert((*(1 in aas)).x == 0)` failed
---
*/


struct S
{
    int x;
    this(int _x){ x = _x; }
    ref S opAssign(int _x){ x = _x; return this; }
}

string gentest_ii(string expr)
{
    return "() { int[int] aai = [ 1 : 2 ]; assert(" ~ expr ~ ");\n return true; }()\n";
}

const ii1 = mixin(gentest_ii("aai[1] == 0"));
const ii2 = mixin(gentest_ii("(aai[1] = 1) == 0"));
const ii3 = mixin(gentest_ii("*(1 in aai) == 3"));
const ii4 = mixin(gentest_ii("aai.remove(2)"));
const ii5 = mixin(gentest_ii("aai != [1:2]"));
const ii6 = mixin(gentest_ii("aai == [1:3]"));

string gentest_is(string expr)
{
    return "() { S[int] aas = [ 1 : S(2) ]; assert(" ~ expr ~ ");\n return true; }()\n";
}

const is1 = mixin(gentest_is("aas[1].x == 0"));
const is2 = mixin(gentest_is("(aas[1] = 1).x == 0"));
const is3 = mixin(gentest_is("(1 in aas).x == 0"));
