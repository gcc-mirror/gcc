// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96157
// { dg-options "-fno-moduleinfo -fno-rtti" }
// { dg-do compile }

struct CodepointSet
{
    CowArray!string data;
}

struct CowArray(SP)
{
    ~this()
    {
        if (data.length)
            refCount;
    }
    @property refCount() { return data[$-1]; }

    uint[] data;
}

int ucmp() { return 1; }

bool loadProperty () {

    CodepointSet target;
    if (ucmp)
        CodepointSet();
    else if (ucmp|| ucmp)
        target = CodepointSet();
    else if (ucmp|| ucmp)
        target = CodepointSet();
    else if (ucmp|| ucmp)
        target = CodepointSet();
    else if (ucmp)
        target = CodepointSet();
    else if (ucmp)
        target = CodepointSet();
    return true;
}
