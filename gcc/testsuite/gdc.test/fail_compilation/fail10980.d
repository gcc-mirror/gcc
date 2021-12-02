/*
TEST_OUTPUT:
---
fail_compilation/fail10980.d(22): Error: variable `fail10980.s1b` of type struct `immutable(S1)` uses `this(this)`, which is not allowed in static initialization
fail_compilation/fail10980.d(28): Error: variable `fail10980.s1d` of type struct `immutable(S1)` uses `this(this)`, which is not allowed in static initialization
fail_compilation/fail10980.d(27): Error: static variable `s1x` cannot be read at compile time
fail_compilation/fail10980.d(28):        called from here: `bar1()`
fail_compilation/fail10980.d(38): Error: variable `fail10980.s2b` of type struct `immutable(S2)` uses `this(this)`, which is not allowed in static initialization
fail_compilation/fail10980.d(44): Error: variable `fail10980.s2d` of type struct `immutable(S2)` uses `this(this)`, which is not allowed in static initialization
fail_compilation/fail10980.d(43): Error: static variable `s2x` cannot be read at compile time
fail_compilation/fail10980.d(44):        called from here: `bar2()`
---
*/

struct S1
{
    this(int) immutable {}
    this(this) {}
}
alias immutable(S1) IS1;
static immutable S1 s1a = IS1(1);   // OK
static immutable S1 s1b = s1a;      // NG

S1 foo1() { S1 s1x; S1 s1y = s1x; return s1y; }
static immutable S1 s1c = foo1();   // OK

ref S1 bar1() { static S1 s1x; return s1x; }
static immutable S1 s1d = bar1();   // NG


struct S2
{
    int val;
    this(this) {}
}
alias immutable(S2) IS2;
static immutable S2 s2a = IS2(1);   // OK
static immutable S2 s2b = s2a;      // NG

S2 foo2() { S2 s2x; S2 s2y = s2x; return s2y; }
static immutable S2 s2c = foo2();   // OK

ref S2 bar2() { static S2 s2x; return s2x; }
static immutable S2 s2d = bar2();   // NG
