// https://issues.dlang.org/show_bug.cgi?id=17143

struct Tuple(T...)
{
    T values;
    alias expand = values;
}

Tuple!T tuple(T...)(T args)
{
    return Tuple!T(args);
}

enum foo = tuple(1, 2).expand;
static assert(typeof(foo).stringof == "(int, int)");
static assert(foo.stringof == "AliasSeq!(1, 2)");
