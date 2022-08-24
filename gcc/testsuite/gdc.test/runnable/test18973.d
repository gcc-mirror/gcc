// This is a runnable test as we are testing a linker error

// https://issues.dlang.org/show_bug.cgi?id=18973
struct X {
    @disable size_t toHash() const;
    @disable string toString() const;
    @disable bool opEquals(const ref X) const;
    @disable int opCmp(const ref X) const;
}

// https://issues.dlang.org/show_bug.cgi?id=9161
public struct dummy
{
    static auto opCall(C)(in C[] name)
    {
        return name;
    }

    @disable ~this(); //comment this out to avoid error
}

void main()
{
    assert(dummy("ABCDE") == "ABCDE");
}
