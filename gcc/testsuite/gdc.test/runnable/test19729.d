//https://issues.dlang.org/show_bug.cgi?id=19729
// PERMUTE_ARGS:
module test19729;

mixin template Templ(T)
{
    this(T t)
    {
    }
}

class C // original TC
{
    mixin Templ!int;
    mixin Templ!string;
}

class D // named
{
    mixin Templ!int ti;
    mixin Templ!string ts;
}

class E // top level ctor wins
{
    bool topLevelWins;
    mixin Templ!int;
    this(int){topLevelWins = true;}
}

class F // top level ctor wins even if not exact match
{
    bool topLevelWins;
    mixin Templ!ubyte;
    this(int){topLevelWins = true;}
}

class G // same as F but change lexicographical order
{
    bool topLevelWins;
    this(int){topLevelWins = true;}
    mixin Templ!ubyte;
}

void main()
{
    auto c0 = new C("should work");
    auto c1 = new C(42);

    auto d0 = new D("should work");
    auto d1 = new D(42);

    auto e = new E(0);
    assert(e.topLevelWins);

    auto f = new F(ubyte(0));
    assert(f.topLevelWins);

    auto g = new G(ubyte(0));
    assert(g.topLevelWins);
}
