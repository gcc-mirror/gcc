void foo()
{
    (void[1]).init.front;
}

void front(T)(T[] a)
{
    static assert(is(T == void));
}
