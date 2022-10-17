// https://issues.dlang.org/show_bug.cgi?id=20909

struct S
{
    long a;
    static assert(x.sizeof == 4);
    enum offset = x.offsetof;
    static assert(offset == 8);   // OK now
    int x;
}
