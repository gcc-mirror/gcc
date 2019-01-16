import imports.test10752;
void main()
{
    static assert(!__traits(compiles, priv));
    static assert(!__traits(compiles, priv));
}
