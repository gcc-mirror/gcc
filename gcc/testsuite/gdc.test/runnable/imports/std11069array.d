module imports.std11069array;

@property bool empty(T)(in T[] a) @safe pure nothrow
{
    return !a.length;
}

void popFront(T)(ref T[] a) @safe pure nothrow
{
    a = a[1 .. $];
}

@property ref T front(T)(T[] a) @safe pure nothrow
{
    return a[0];
}
