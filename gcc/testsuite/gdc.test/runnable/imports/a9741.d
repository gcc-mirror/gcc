module imports.a9741b;

template ShowAttributes(alias X)
{
    pragma(msg, X.stringof);
    pragma(msg, __traits(getAttributes, X));
}
