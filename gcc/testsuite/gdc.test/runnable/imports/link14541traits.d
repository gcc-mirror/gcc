module imports.link14541traits;

template hasElaborateAssign(S)
{
    static if (is(S == struct))
    {
        extern __gshared S lvalue;

        enum hasElaborateAssign = is(typeof(S.init.opAssign(S.init))) ||
                                  is(typeof(S.init.opAssign(lvalue)));
    }
    else
    {
        enum bool hasElaborateAssign = false;
    }
}

void swap(T)(ref T lhs, ref T rhs) @trusted pure nothrow @nogc
{
    static if (hasElaborateAssign!T)
    {
    }
    else
    {
    }
}

template Tuple(Types...)
{
    struct Tuple
    {
        Types field;
        alias field this;

        this(Types values)
        {
            field[] = values[];
        }

        void opAssign(R)(auto ref R rhs)
        {
            static if (is(R : Tuple!Types) && !__traits(isRef, rhs))
            {
                // Use swap-and-destroy to optimize rvalue assignment
                swap!(Tuple!Types)(this, rhs);
            }
            else
            {
                // Do not swap; opAssign should be called on the fields.
                field[] = rhs.field[];
            }
        }
    }
}
