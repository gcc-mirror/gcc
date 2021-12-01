struct A
{
    void foo(T)(T t) {}
    void foo(long l) {}

    void bar(long l) {}
    void bar(T)(T t) {}
}

static assert(__traits(getOverloads, A, "foo").length == 1);
static assert(__traits(getOverloads, A.init, "foo").length == 1);

static assert(__traits(getOverloads, A, "foo", true).length == 2);
static assert(__traits(getOverloads, A.init, "foo", true).length == 2);

static assert(__traits(getOverloads, A, "bar").length == 1);
static assert(__traits(getOverloads, A.init, "bar").length == 1);

static assert(__traits(getOverloads, A, "bar", true).length == 2);
static assert(__traits(getOverloads, A.init, "bar", true).length == 2);
