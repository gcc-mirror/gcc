// https://issues.dlang.org/show_bug.cgi?id=21543

class B
{
    Nullable!B data;
    alias data this;
}

void test1()
{
    B b;
    Nullable!B n;
}

struct Nullable(T)
{
    T payload;

    void opAssign()(T)
    {
        move(payload);
    }

    inout(T) get_() inout
    {
        return payload;
    }

    alias get_ this;
}

// another version with chain of 3 alias this

struct C
{
    Nullable2 data;
    alias data this;
}

void test2()
{
    C c;
    Nullable2 n2 = &c;
    Nullable3 n3 = &c;

    // these are to check a sane -vcg-ast output
    fn1(c);
    fn1(n2);
    fn1(n3);
    fn2(c);
    fn2(n2);
    fn2(n3);
    fn3(c);
    fn3(n2);
    fn3(n3);
}

void fn1(C x) {}

void fn2(Nullable2 x) {}

void fn3(Nullable3 x) {}

struct Nullable2
{
    Nullable3 payload;

    this(C* c)
    {
        payload = Nullable3(c);
    }

    void opAssign()(Nullable3)
    {
        move(payload);
    }

    inout(Nullable3) get_() inout
    {
        return payload;
    }

    alias get_ this;
}

struct Nullable3
{
    C* payload;

    this(C* c)
    {
        payload = c;
    }

    void opAssign()(C)
    {
        move(payload);
    }

    inout(C) get_() inout
    {
        return *payload;
    }

    alias get_ this;
}

T move(T)(ref T source)
{
    return source;
}

T move(T)(T source)
{
    return source;
}
