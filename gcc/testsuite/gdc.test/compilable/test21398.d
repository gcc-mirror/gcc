// https://issues.dlang.org/show_bug.cgi?id=21398

module test21398;

void free(void* ptr);

class MAlloc(T)
{
    import test21398: free;

    void free(T)(T* value)
    {
        free(value);
    }
}

struct Box(T)
{
    private T* __ptr;
    alias A = MAlloc!T;

    ~this()
    {
        A.free(__ptr);
    }
}

void main()
{
    auto b = Box!(char)();
}
