module imports.test18868_fls;

template FLS(T)
{
    int ctorcount = 0;
    int dtorcount = 0;
    int sharedctorcount = 0;
    int shareddtorcount = 0;

    static this()
    {
        assert(ctorcount == 0);
        ctorcount += 1;
    }

    static ~this()
    {
        assert(dtorcount == 0);
        dtorcount += 1;
    }

    shared static this()
    {
        assert(sharedctorcount == 0);
        sharedctorcount += 1;
    }

    shared static ~this()
    {
        assert(shareddtorcount == 0);
        shareddtorcount += 1;
    }
}
