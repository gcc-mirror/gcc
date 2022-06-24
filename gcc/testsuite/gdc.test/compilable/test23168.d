// https://issues.dlang.org/show_bug.cgi?id=23168
// Issue 23168 - [DIP1000] return scope wrongly rewritten for structs with no indirections

@safe:
struct Ptr
{
    int* fun() return scope { return null; }
}

int* funf(ref return scope Ptr p) { return null; }

int* use()
{
    Ptr ptr;
    return ptr.fun;
    return funf(ptr);
}

// Prevent forward reference 'regression'
// See https://github.com/dlang/dmd/pull/14232#issuecomment-1162906573
struct S
{
    void f() scope {}
    alias x = _get_value;

    static if (true)
        int _get_value() {return 3;}
    else
        int _get_value() {return 4;}
}
