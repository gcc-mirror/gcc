
extern (C) struct S { }

version (CRuntime_Microsoft)
    static assert(S.sizeof == 4);
else
    static assert(S.sizeof == 0);

version (CRuntime_DigitalMars)
    static assert(S.alignof == 0);
else
    static assert(S.alignof == 1);

extern (C++) struct T { }

static assert(T.sizeof == 1);
static assert(T.alignof == 1);
