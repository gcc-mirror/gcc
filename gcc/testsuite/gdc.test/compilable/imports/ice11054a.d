template Tuple()
{
    string injectNamedFields()
    {
        formatNthX();
        return "";
    }
}
Tuple!T tuple(T...)()
{
}

void formatNthX(A...)(A)
{
    static gencode(size_t count)()
    {
        result ~= "";   // comment out this line will remove the ICE
        return "";
    }
    mixin(gencode!(A.length)());
}
