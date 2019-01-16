// PERMUTE_ARGS:

struct Field
{
    ~this() @safe @nogc pure nothrow {}
}

struct Counter
{
    static size_t cnt;
    ~this() @safe @nogc nothrow { ++cnt; }
}

struct Foo
{
    ~this() @safe @nogc pure nothrow {}
    Field field;
}

class Bar
{
    ~this() @safe @nogc pure nothrow {}
    Field field;
}

void test1() @nogc pure nothrow
{
    Foo foo;
    foo.__xdtor();
    scope bar = new Bar();
    bar.__xdtor();
}

static assert(__traits(hasMember, Foo, "__xdtor"));
static assert(__traits(hasMember, Bar, "__xdtor"));

//

struct FieldDtor
{
    Counter counter;
}

struct AggrDtor
{
    static size_t cnt;
    ~this() @safe @nogc nothrow { ++cnt; }
}

struct MixedDtor
{
    static size_t cnt;
    Counter counter;
    ~this() @safe @nogc nothrow { ++cnt; }
}

struct SNoDtor {}
class CNoDtor {}

static assert(!__traits(hasMember, SNoDtor, "__xdtor"));
static assert(!__traits(hasMember, CNoDtor, "__xdtor"));

void test2() @safe @nogc nothrow
{
    FieldDtor a;
    assert(Counter.cnt == 0);
    a.__xdtor();
    assert(Counter.cnt == 1);
    AggrDtor b;
    assert(AggrDtor.cnt == 0);
    b.__xdtor();
    assert(AggrDtor.cnt == 1);
    Counter.cnt = 0;
    MixedDtor c;
    assert(MixedDtor.cnt == 0);
    assert(Counter.cnt == 0);
    c.__xdtor();
    assert(MixedDtor.cnt == 1);
    assert(Counter.cnt == 1);
}

void main()
{
    test1();
    test2();
}
