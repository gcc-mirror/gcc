// https://issues.dlang.org/show_bug.cgi?id=22421

alias AliasSeq(T...) = T;

template staticMap(alias fun, args...)
{
    alias staticMap = AliasSeq!();
    static foreach(arg; args)
        staticMap = AliasSeq!(staticMap, fun!arg);
}

template id(alias what)
{
    enum id = __traits(identifier, what);
}

enum A { a }

static assert(staticMap!(id, A.a) == AliasSeq!("a"));
