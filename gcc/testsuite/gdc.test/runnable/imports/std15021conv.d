module imports.std15021conv;

T to(T, A...)(A args)
{
    return toImpl!T(args);
}

T toImpl(T, S)(S value)
{
    import imports.std15021format;
    enforceValidFormatSpec!(S, char)('c');
    return "";
}
