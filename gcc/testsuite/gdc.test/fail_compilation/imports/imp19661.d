template isFunction(X...)
if (X.length == 1)
{
    static if (is(typeof(&X[0]) U : U*) && is(U == function) ||
               is(typeof(&X[0]) U == delegate))
    {
        // x is a (nested) function symbol.
        enum isFunction = true;
    }
    else static if (is(X[0] T))
    {
        // x is a type.  Take the type of it and examine.
        enum isFunction = is(T == function);
    }
    else
        enum isFunction = false;
}
