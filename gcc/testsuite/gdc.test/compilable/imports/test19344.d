module imports.test19344;

template getUDAs(alias symbol, alias attribute)
{
    alias getUDAs = __traits(getAttributes, symbol);
}
