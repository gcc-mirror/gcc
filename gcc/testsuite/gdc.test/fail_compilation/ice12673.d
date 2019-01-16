void main()
{
    static assert(__traits(compiles, { abcd(); }));
}
