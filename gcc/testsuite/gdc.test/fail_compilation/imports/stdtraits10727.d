template StaticArrayTypeOf(T)
{
    inout(U[n]) idx(U, size_t n)( inout(U[n]) );

    /+static if (is(T == enum))
        alias .StaticArrayTypeOf!(OriginalType!T) StaticArrayTypeOf;
    else +/static if (is(typeof(idx(T.init/+defaultInit!T+/)) X))
        alias X StaticArrayTypeOf;
    else
        static assert(0, T.stringof~" is not a static array type");
}

template DynamicArrayTypeOf(T)
{
    inout(U[]) idx(U)( inout(U[]) );

    /+static if (is(T == enum))
        alias .DynamicArrayTypeOf!(OriginalType!T) DynamicArrayTypeOf;
    else +/static if (!is(StaticArrayTypeOf!T) &&
                     is(typeof(idx(T.init/+defaultInit!T+/)) X))
    {
        alias typeof(T.init[0]/+defaultInit!T[0]+/) E;

                     E[]  idy(              E[]  );
               const(E[]) idy(        const(E[]) );
               inout(E[]) idy(        inout(E[]) );
        shared(      E[]) idy( shared(      E[]) );
        shared(const E[]) idy( shared(const E[]) );
        shared(inout E[]) idy( shared(inout E[]) );
           immutable(E[]) idy(    immutable(E[]) );

        alias typeof(idy(T.init/+defaultInit!T+/)) DynamicArrayTypeOf;
    }
    else
        static assert(0, T.stringof~" is not a dynamic array");
}

template isDynamicArray(T)
{
    enum isDynamicArray = is(DynamicArrayTypeOf!T)/+ && !isAggregateType!T+/;
}

template isArray(T)
{
    enum bool isArray = /+isStaticArray!T || +/isDynamicArray!T;
}
