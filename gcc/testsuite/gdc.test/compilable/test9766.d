// PERMUTE_ARGS:

size_t getAlign9766(size_t n) { return n; }

struct S9766
{
align(getAlign9766(1)):
    ubyte[5] pad1;
    ubyte var1;

align(getAlign9766(2)):
    ubyte[5] pad2;
    ubyte var2;

align(getAlign9766(4)):
    ubyte[5] pad3;
    ubyte var3;

align(getAlign9766(8)):
    ubyte[5] pad4;
    ubyte var4;
}

static assert(S9766.pad1.offsetof == 0);
static assert(S9766.var1.offsetof == 5);

static assert(S9766.pad2.offsetof == 6);
static assert(S9766.var2.offsetof == 12);

static assert(S9766.pad3.offsetof == 16);
static assert(S9766.var3.offsetof == 24);

static assert(S9766.pad4.offsetof == 32);
static assert(S9766.var4.offsetof == 40);

union U9766
{
    struct
    {
    align(getAlign9766(1)):
        ubyte[5] pad1;
        ubyte var1;

    align(getAlign9766(2)):
        ubyte[5] pad2;
        ubyte var2;

    align(getAlign9766(4)):
        ubyte[5] pad3;
        ubyte var3;

    align(getAlign9766(8)):
        ubyte[5] pad4;
        ubyte var4;
    }
}

static assert(U9766.pad1.offsetof == 0);
static assert(U9766.var1.offsetof == 5);

static assert(U9766.pad2.offsetof == 6);
static assert(U9766.var2.offsetof == 12);

static assert(U9766.pad3.offsetof == 16);
static assert(U9766.var3.offsetof == 24);

static assert(U9766.pad4.offsetof == 32);
static assert(U9766.var4.offsetof == 40);

struct TestMaxAlign
{
align(1u << 15):
    ubyte a;
    ubyte b;
}

static assert(TestMaxAlign.b.offsetof == (1 << 15));
