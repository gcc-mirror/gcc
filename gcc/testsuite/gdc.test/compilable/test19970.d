// https://issues.dlang.org/show_bug.cgi?id=19970

enum void* p = cast(void*)0;
static assert(p is null);
static assert(ctfeLocal(p));
static assert(ctfeGlobal());

bool ctfeGlobal ()
{
    return p is null;
}

bool ctfeLocal (const void* ptr) pure
{
    return ptr is null;
}
