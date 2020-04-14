// { dg-do run { target { hw && d_runtime_has_std_library } } }

module runnable;

import core.stdc.stdio;
import gcc.attribute;


/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=2

struct S
{
    string toString() { return "foo"; }
}

void test2()
{
    import std.string : format;
    assert(format("%s", S()) == "foo");
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=15

class B
{
    class A { }
    A a;
}

class C
{
    void visit(B b)
    {
        import std.algorithm : map;
        auto as = [b.a];
        as.map!(d => d);
    }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=16

void test16()
{
    import std.parallelism : taskPool;

    taskPool.reduce!"a+b"([0, 1, 2, 3]);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=18

class C18
{
    struct Link
    {
        int x;
        int y;
    }

    void sort_links()
    {
        import std.algorithm : sort;
        import std.array : empty;
        import std.exception : enforce;

        enforce(!_link.empty);

        bool lt(Link a, Link b)
        {
            if(a.x > b.x)
                return false;
            if(a.x < b.x)
                return true;
            if(a.y >= b.y)
                return false;
            else
                return true;
        }
        sort!(lt)(_link);
    }

    this()
    {
        _link ~= Link(8, 3);
        _link ~= Link(4, 7);
        _link ~= Link(4, 6);
        _link ~= Link(3, 7);
        _link ~= Link(2, 7);
        _link ~= Link(2, 2);
        _link ~= Link(4, 1);
    }

    Link[] _link;
}

void test18()
{
    C18 foo = new C18;
    foo.sort_links();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=29

void test29()
{
    import std.string : format;
    import std.conv : text;

    string s;
    for (auto i = 0; i < 100000; i++)
    {
        s = format("%d", i);
        s = text(i);
    }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=52

class C52
{
    C52 a;

    this()
    {
        printf("Construct: this=%p\n", cast(void*)this);
        a = this;
    }

    bool check()
    {
        printf("Check: this=%p a=%p\n", cast(void*)this, cast(void*)a);
        return this is a;
    }
}

auto test52a()
{
    import std.conv, std.traits;

    struct Scoped
    {
        void[__traits (classInstanceSize, C52) ] Scoped_store = void;

        inout(C52) Scoped_payload() inout
        {
            void* alignedStore = cast(void*) Scoped_store.ptr;
            return cast(inout (C52)) alignedStore;
        }
        alias Scoped_payload this;
    }

    Scoped result;
    emplace!(Unqual!C52)(result.Scoped_store);
    assert(result.Scoped_payload().check);
    return result;
}

void test52()
{
    auto a1 = test52a();
    assert(a1.Scoped_payload().check);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=71

struct Leaf
{
    ubyte symbol;
    ubyte codeLen;
}

struct CanonicalHuffman
{
    Leaf[] table;

    void print()
    {
        import std.algorithm;
        import std.range;

        auto list = zip(iota(table.length), table.dup).array
            .sort!((a, b) => a[1].symbol < b[1].symbol)
            .uniq!((a, b) => (a[0] & (1 << a[1].codeLen) - 1) == (b[0] & (1 << b[1].codeLen) - 1));
    }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=77

void fun(ubyte[3] buf)
{
    import std.bitmanip : bigEndianToNative;
    bigEndianToNative!ushort(buf[0..2]);
}

void test77()
{
    fun([1,2,3]);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=108

@attribute("forceinline")
void test108()
{
    import std.stdio : writeln;
    writeln("Here");
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=122

void test122()
{
    import std.algorithm : map;
    import std.parallelism : taskPool;
    import std.range : iota;

    immutable n = 10000;
    enum delta = 1.0 / n;       // XBUG: was 'immutable delta' https://issues.dlang.org/show_bug.cgi?id=17092
    immutable pi = 4.0 * delta * taskPool.reduce!"a + b"(
        map!((int i) { immutable x = (i - 0.5) * delta; return 1.0 / (1.0 + x * x); })(iota(n)));
}

/******************************************/

void main()
{
    test2();
    test16();
    test18();
    test52();
    test77();
    test108();

    printf("Success!\n");
}
