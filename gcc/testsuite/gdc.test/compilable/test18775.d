// REQUIRED_ARGS: -de

struct Foo { }

struct Bar {
    deprecated
    @property Foo foo() { return Foo.init; }

    alias foo this;
}

void test(Bar bar) { }

void main()
{
    Bar bar;

    // test lookup will be satisfied via ufcs, not alias, so it must not deprecation warn foo!
    bar.test;
}
