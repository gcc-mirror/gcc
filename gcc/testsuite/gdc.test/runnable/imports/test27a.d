module imports.test27a;

struct Variant
{
    this(T)(T)
    {
    }
}

class myClass(T)
{
public:
    void func(T v)
    {
        Variant b = Variant(v);
    }
}
