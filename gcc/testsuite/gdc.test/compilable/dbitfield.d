/* REQUIRED_ARGS: -preview=bitfields
 */

/***************************************************/

class C
{
    uint a:3;
    uint b:1;
    ulong c:64;

    int d:3;
    int e:1;
    long f:64;

    int i;
}

static assert(C.a.min == 0);
static assert(C.a.max == 7);

static assert(C.b.min == 0);
static assert(C.b.max == 1);

static assert(C.c.min == 0);
static assert(C.c.max == ulong.max);

static assert(C.d.min == -4);
static assert(C.d.max == 3);

static assert(C.e.min == -1);
static assert(C.e.max == 0);

static assert(C.f.min == long.min);
static assert(C.f.max == long.max);

int testc()
{
    scope c = new C();
    c.d = 9;
    return c.d;
}

static assert(testc() == 1);

/***************************************************/

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

static assert(U.sizeof == 8);

static assert(U.a.min == 0);
static assert(U.a.max == 7);

static assert(U.b.min == 0);
static assert(U.b.max == 1);

static assert(U.c.min == 0);
static assert(U.c.max == ulong.max);

static assert(U.d.min == -4);
static assert(U.d.max == 3);

static assert(U.e.min == -1);
static assert(U.e.max == 0);

static assert(U.f.min == long.min);
static assert(U.f.max == long.max);

int testu()
{
    U u;
    u.d = 9;
    return u.d;
}

static assert(testu() == 1);
