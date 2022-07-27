////////////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=15478

void test15478_1()
{
    struct Foo(N)
    {
        this(N value) { }
        static int bug() { return 0; }
    }
    enum Foo!int foo = 0;
    Foo!int[foo.bug] bar;
}

void test15478_2()
{
    int getLength()  { return 42; }
    struct Get {static int length() { return 42; }}

    int[getLength]  i1;
    int[Get.length] i2;
    static assert (is(typeof(i1) == int[42]));
    static assert (is(typeof(i2) == int[42]));
}

////////////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=21870
struct S21870
{
    @property size_t count() const
    {
        return 1;
    }
}

int[S21870.init.count()] x; // OK
int[S21870.init.count  ] y; // error

////////////////////////////////////////////////////////////////////////////////

struct Foo15478(N)
{
    this(N value) { }
    auto bug() { return 0; }
}

void test15478_3()
{
    enum Foo15478!int foo = 0;
    Foo15478!int[foo.bug] bar; // Error: integer constant expression expected instead of Foo().bug

    enum foo_bug = foo.bug;
    Foo15478!int[foo_bug] baz; // OK
}
