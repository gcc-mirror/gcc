// https://issues.dlang.org/show_bug.cgi?id=23087
struct S
{
    this(bool) {}
    this(bool, int) {}
}

static foreach (ctor; __traits(getOverloads, S, "__ctor"))
    static assert(__traits(getLinkage, ctor) == "D");
