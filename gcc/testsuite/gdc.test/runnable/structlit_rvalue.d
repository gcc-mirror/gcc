void refCounted(ProcessPipes val)
{
    val = ProcessPipes.init;
}

struct ProcessPipes
{
    char[33] arr;
    ~this() @safe {}
}

void main()
{
    refCounted(ProcessPipes.init);
}
