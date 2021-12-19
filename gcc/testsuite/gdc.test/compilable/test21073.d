// https://issues.dlang.org/show_bug.cgi?id=21073

class C
{
    auto internal() const
    {
        return 5;
    }
    alias internal this;
}

void main() pure
{
    const c = new C;
    auto r = cast(C)c;
}
