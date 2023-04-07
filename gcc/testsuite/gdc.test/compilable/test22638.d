// https://issues.dlang.org/show_bug.cgi?id=22638

struct S
{
    this(ref const(S));
    ~this();
}

extern(C++) void set(const S s);

void disp()
{
    S p;
    return set(p);
}
