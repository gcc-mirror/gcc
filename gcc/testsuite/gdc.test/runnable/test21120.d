// https://issues.dlang.org/show_bug.cgi?id=21120

module one.two.three;

struct S {}

struct StructTemplate(T)
{
    int a = 123; // non-zero initialized

    ref const(StructTemplate) getInitSymbol()
    {
        return initSymbol!StructTemplate;
    }
}

template initSymbol(T)
{
    pragma(mangle, "_D" ~ T.mangleof[1..$] ~ "6__initZ")
    extern immutable T initSymbol;
}

void main()
{
    StructTemplate!S inst;
    assert(inst.getInitSymbol() == inst);
}
