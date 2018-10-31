// 6999: inout in front of return type

struct A
{
inout:
    inout(int) foo()
    {
        return 0;
    }
}

struct B
{
    inout
    {
        inout(int) foo()
        {
            return 0;
        }
    }
}

struct C
{
    inout inout(int) foo()
    {
        return 0;
    }
}
