module imports.bug10425;

struct A()
{
    int opCmp(const ref A p) const
    {
        return 0;
    }

    string toString()
    {
        return "";
    }
}

struct B()
{
    void foo()
    {
        auto a = new A!();
    }
}

struct C
{
    alias A!() a_t;

    this(B!() b)
    {

    }
}
