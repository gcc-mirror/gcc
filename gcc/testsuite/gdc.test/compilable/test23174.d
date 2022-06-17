// https://issues.dlang.org/show_bug.cgi?id=23174

alias AliasSeq(T...) = T;

template staticMap(alias fun, args...)
{
    alias staticMap = AliasSeq!();
    static foreach(arg; args)
        staticMap = AliasSeq!(staticMap, fun!arg);
}

template Filter(alias pred, args...)
{
    alias Filter = AliasSeq!();
    static foreach (arg; args)
        static if (pred!arg)
            Filter = AliasSeq!(Filter, arg);
}

struct Fields(T)
{
    private static alias toField(alias e) = Field!(__traits(identifier, e));
    alias fields = staticMap!(toField, T.tupleof);

    static alias map(alias F) = staticMap!(F, fields);
    static alias filter(alias pred) = Filter!(pred, fields);
}

struct Field(string n)
{
    enum name = n;
}

struct Foo
{
    int a;
}

void test23174()
{
    Foo value;

    enum toName(alias e) = e.name;
    enum pred(alias e) = true;

    alias a = Fields!(Foo).filter!(pred); // works
    static assert(is(a == AliasSeq!(Field!"a")));

    alias b = Fields!(Foo).map!(toName); // works
    static assert(b == AliasSeq!("a"));

    alias c = Fields!(Foo).init.filter!(pred); // works
    static assert(is(c == AliasSeq!(Field!"a")));

    // OK <- Error: alias `d` cannot alias an expression `Fields().tuple("a")`
    alias d = Fields!(Foo).init.map!(toName);
    static assert(d == AliasSeq!("a"));
}
