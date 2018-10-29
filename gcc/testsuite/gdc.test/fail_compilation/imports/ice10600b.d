module imports.ice10600b;

import imports.ice10600a;

template to(T)
{
    T to(A...)(A args)
    {
        return toImpl!T(args);
    }
}


T toImpl(T, S)(S value)
if (is(S : T))
{
    return value;
}


T toImpl(T, S)(S value)
if (!is(S : T) &&
     is(T == string))
{
    auto w = appender!T();
    //Appender!T w;
    return null;
}

T toImpl(T, S)(S value)
if ( is(S == string) &&
    !is(T == string) && is(typeof(to!string(value[0])))
   )
{
    return T.init;
}
