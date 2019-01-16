
version(GNU)
{
    static assert(0);
}
version(Win64)
{
    static assert(0);
}
else version(X86_64)
{
    void error(...){}
}
else
{
    static assert(0);
}

