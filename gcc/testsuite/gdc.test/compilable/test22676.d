// https://issues.dlang.org/show_bug.cgi?id=22676
template fullyQualifiedName(T)
{
    static if (is(T : real))
        enum fullyQualifiedName;

    enum fullyQualifiedName = null;
}

static auto _inst()
{
    return fullyQualifiedName!(frop);
}

alias attr = __traits(getAttributes, _inst);

class frop
{
    alias type_id = registry!frop;
}

template registry(T)
{
    enum string FOO = fullyQualifiedName!T;
}
