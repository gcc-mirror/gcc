// https://issues.dlang.org/show_bug.cgi?id=21659

// Compiler-recognized ident
enum __c_ulonglong : ulong;

private union EndianSwapper(T)
{
    T value;
    ubyte[T.sizeof] array;
    static assert(T.sizeof == ulong.sizeof);
}

void main ()
{
    EndianSwapper!(__c_ulonglong) val;
}
