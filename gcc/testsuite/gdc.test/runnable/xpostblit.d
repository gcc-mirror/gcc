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

/****************************************************************
 This test is intended to verify the exception safety of field
 postblits
*/
string trace = "";

struct FieldThrow
{
    string name;
    this(string n)
    {
        name = n;
    }

    bool throwExcept;
    this(this)
    {
        if (throwExcept)
        {
            throw new Exception("");
        }
    }

    ~this() { trace ~= name ~ ".dtor"; }
}

struct S
{
    auto f1 = FieldThrow("f1");
    FieldThrow[2] f2f3= [FieldThrow("f2"), FieldThrow("f3")];
    auto f4 = FieldThrow("f4");
}

void test3()
{
    trace = "";

    S s1;

    // Cause `s1.f4`'s postblit to throw
    s1.f4.throwExcept = true;

    try
    {
        // `s`'s postblit will be a combination of `f1`, `f2f3`, and `f4`'s
        // postblit in that order.  However, `f4`'s postblit will throw,
        // causing `s1.f2f3` and `s1.f1`'s destructors to execute in that
        // order
        S s2 = s1;
    }
    catch(Exception ex){ }

    // Confirm the field destructors were called and were called in the
    // corrrect order
    assert(trace == "f3.dtor" ~ "f2.dtor" ~ "f1.dtor");
}
/****************************************************************************/

void main()
{
    test1();
    test2();
    test3();
}
