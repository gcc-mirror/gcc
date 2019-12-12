// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

template isCallable(T...)
{
    static if (is(typeof(& T[0].opCall) == delegate))
    {
        enum bool isCallable = true;
    }
    else static if (is(typeof(& T[0].opCall) V : V*) && is(V == function))
    {
        enum bool isCallable = true;
    }
    else
        enum bool isCallable = false;
}

@property auto injectChain(Injectors...)()
{
    return &ChainTemplates!(Injectors);
}

template ChainTemplates(Templates...)
{
    alias Head = Templates[0];
    alias Tail = Templates[1..$];
    alias Head!(Tail) ChainTemplates;
}

static assert(!isCallable!(injectChain));
