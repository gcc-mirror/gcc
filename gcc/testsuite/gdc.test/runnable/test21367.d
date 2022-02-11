// https://issues.dlang.org/show_bug.cgi?id=21367

string result = "";

struct RCArray(T)
{
    T* data;
    this(this)
    {
        result ~= "A";
    }
    ~this()
    {
        result ~= "B";
    }
}

struct Variant(T...)
{
    union
    {
        T payload;
    }
    this(this)
    {
        result ~= "C";
    }

    ~this()
    {
        result ~= "D";
    }
}

alias Ft = Variant!(RCArray!double, RCArray!int);

void fun(Ft a) {}
void main()
{
    Ft a;
    Ft b = a;
}

static ~this()
{
    assert(result == "CDD");
}
