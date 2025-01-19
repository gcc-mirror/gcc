/* REQUIRED_ARGS: -preview=bitfields
 */

struct S
{
    int a;
    int b:5, c:6;
}

static if (0)
{
    pragma(msg, __traits(isBitfield, S.a));
    pragma(msg, __traits(isBitfield, S.b));
    pragma(msg, S.b.bitoffsetof);
    pragma(msg, S.b.bitwidth);
    pragma(msg, S.c.bitoffsetof);
    pragma(msg, S.c.bitwidth);
    pragma(msg, S.a.bitoffsetof);
    pragma(msg, S.a.bitwidth);
}

static assert(__traits(isBitfield, S.a) == false);
static assert(__traits(isBitfield, S.b) == true);
static assert(S.b.bitoffsetof == 0);
static assert(S.b.bitwidth == 5);
static assert(S.c.bitoffsetof == 5);
static assert(S.c.bitwidth == 6);

/* TEST_OUTPUT:
---
fail_compilation/bitintro.d(6): Error: `a` is not a bitfield, cannot apply `bitoffsetof`
fail_compilation/bitintro.d(37):        while evaluating: `static assert(a.bitoffsetof)`
fail_compilation/bitintro.d(6): Error: `a` is not a bitfield, cannot apply `bitwidth`
fail_compilation/bitintro.d(38):        while evaluating: `static assert(a.bitwidth)`
---
*/
static assert(S.a.bitoffsetof);
static assert(S.a.bitwidth);
