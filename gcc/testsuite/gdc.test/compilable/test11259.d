// https://issues.dlang.org/show_bug.cgi?id=11259

version (Posix)
{
    // smallest druntime module without imports on posix
    import core.sys.posix.libgen;
    static assert(__traits(isSame, __traits(parent, core.sys.posix.libgen), core.sys.posix));
    static assert(__traits(isSame, core.sys.posix, __traits(parent, core.sys.posix.libgen)));

    static assert(__traits(isSame, __traits(parent, core.sys.posix), core.sys));
    static assert(__traits(isSame, core.sys, __traits(parent, core.sys.posix)));
}
else
{
    // smallest module without imports for windows
    import core.sys.windows.lmuseflg;
    static assert(__traits(isSame, __traits(parent, core.sys.windows.lmuseflg), core.sys.windows));
    static assert(__traits(isSame, core.sys.windows, __traits(parent, core.sys.windows.lmuseflg)));

    static assert(__traits(isSame, __traits(parent, core.sys.windows), core.sys));
    static assert(__traits(isSame, core.sys, __traits(parent, core.sys.windows)));
}

static assert(__traits(isSame, __traits(parent, core.sys), core));
static assert(__traits(isSame, core, __traits(parent, core.sys)));
