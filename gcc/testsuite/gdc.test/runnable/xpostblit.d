// PERMUTE_ARGS:

struct Field
{
    this(this) @safe @nogc pure nothrow {}
}

struct Counter
{
    static size_t cnt;
    this(this) @safe @nogc nothrow { ++cnt; }
}

struct Foo
{
    this(this) @safe @nogc pure nothrow {}
    Field field;
}

void test1() @safe @nogc pure nothrow
{
    Foo foo;
    foo.__xpostblit();
}

static assert(__traits(hasMember, Foo, "__xpostblit"));

//

struct FieldPostblit
{
    Counter counter;
}

struct AggrPostblit
{
    static size_t cnt;
    this(this) @safe @nogc nothrow { ++cnt; }
}

struct MixedPostblit
{
    static size_t cnt;
    Counter counter;
    this(this) @safe @nogc nothrow { ++cnt; }
}

struct SNoPostblit {}
class CNoPostblit {}

static assert(!__traits(hasMember, SNoPostblit, "__xpostblit"));
static assert(!__traits(hasMember, CNoPostblit, "__xpostblit"));

void test2() @safe @nogc nothrow
{
    FieldPostblit a;
    assert(Counter.cnt == 0);
    a.__xpostblit();
    assert(Counter.cnt == 1);
    AggrPostblit b;
    assert(AggrPostblit.cnt == 0);
    b.__xpostblit();
    assert(AggrPostblit.cnt == 1);
    Counter.cnt = 0;
    MixedPostblit c;
    assert(MixedPostblit.cnt == 0);
    assert(Counter.cnt == 0);
    c.__xpostblit();
    assert(MixedPostblit.cnt == 1);
    assert(Counter.cnt == 1);
}

void main()
{
    test1();
    test2();
}
