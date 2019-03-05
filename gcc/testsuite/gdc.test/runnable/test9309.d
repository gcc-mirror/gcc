immutable(T)[] assumeUnique(T)(ref T[] array) pure nothrow
{
    auto result = cast(immutable(T)[]) array;
    array = null;
    return result;
}

pure nothrow
private string escapeShellArguments()
{
    char[] buf;

    @safe nothrow
    char[] allocator(size_t size)
    {
        return buf = new char[size];
    }

    escapeShellArgument!allocator("foo");
    return assumeUnique(buf);
}

@safe nothrow
auto escapeShellArgument(alias allocator)(in char[] arg)
{
    auto buf = allocator(4);
    buf[0] = 'f';
    buf[1] = 'o';
    buf[2] = 'o';
    buf[3] = '\0';
}

void main()
{
    string res = escapeShellArguments();
    if (res != "foo\0") assert(0);
}
