/* REQUIRED_ARGS: -preview=bitfields
 */

struct T
{
    uint x : 2, y : 3, :0;
    int :0;
}

uint foo(T s)
{
    return s.x + s.y;
}

void test1()
{
    T s;
    s.x = 2;
    s.y = 4;
    uint u = foo(s);
    assert(u == 6);
}

/********************************************/

struct S
{
    uint a:3;
    uint b:1;
    ulong c:64;

    int d:3;
    int e:1;
    long f:64;

    int i;
    alias f this;
}

static assert(S.a.min == 0);
static assert(S.a.max == 7);

static assert(S.b.min == 0);
static assert(S.b.max == 1);

static assert(S.c.min == 0);
static assert(S.c.max == ulong.max);

static assert(S.d.min == -4);
static assert(S.d.max == 3);

static assert(S.e.min == -1);
static assert(S.e.max == 0);

static assert(S.f.min == long.min);
static assert(S.f.max == long.max);
static assert(S.max == S.f.max);

void test2()
{
    int x;
    S effect()
    {
        ++x;
        return S();
    }
    assert(effect().a.max == 7);
    assert(effect().i.max == int.max);
    assert(x == 0); // ensure effect() was not executed
}

/********************************************/

struct U
{
    int a;
    int b:3, c:4;
    this(this)
    {
	b = 2;
    }
}

static assert(U.b.offsetof == 4);
static assert(U.b.sizeof == 4);

void test3()
{
    U u;
    u.c = 4;
    U v = u;
    assert(v.c == 4);
    u = v;
    assert(u.b == 2);
    assert(__traits(getMember, u, "b") == 2);
}

/********************************************/

int main()
{
    test1();
    test2();
    test3();
    return 0;
}
