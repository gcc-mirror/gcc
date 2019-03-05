module imports.a15667;

template staticIndexOf(T)
{
    enum staticIndexOf = genericIndexOf!T;
}

template genericIndexOf(args...)
{
    alias e     = args;
    alias tuple = args;
    alias tail = tuple;
    enum next  = genericIndexOf!(e, tail);
}

alias X = ;

static if (staticIndexOf!X)
