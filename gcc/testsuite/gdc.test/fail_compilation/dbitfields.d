/* REQUIRED_ARGS: -preview=bitfields
 * TEST_OUTPUT:
---
fail_compilation/dbitfields.d(118): Error: reinterpretation through overlapped field `e` is not allowed in CTFE
fail_compilation/dbitfields.d(121):        called from here: `testu()`
fail_compilation/dbitfields.d(121):        while evaluating: `static assert(testu() == 1)`
---
 */

#line 100

union U
{
    uint a:3;
    uint b:1;
    ulong c:64;

    int d:3;
    int e:1;
    long f:64;

    int i;
}

int testu()
{
    U u;
    u.d = 9;
    return u.e;
}

static assert(testu() == 1);
