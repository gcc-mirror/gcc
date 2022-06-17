// https://issues.dlang.org/show_bug.cgi?id=22997

struct Forward {}

struct Foo
{
    this(ref typeof(this) rhs)
    {
        this(rhs, Forward.init);
    }

    this(ref typeof(this) rhs, Forward) {}
    this(typeof(this) rhs, int i, double d, string s) {}
}
