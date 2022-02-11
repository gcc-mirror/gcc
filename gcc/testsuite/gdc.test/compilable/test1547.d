// https://issues.dlang.org/show_bug.cgi?id=1547

struct A
{
    int b;
    static A opCall(int k)
    {
        A a;
        a.b = k;
        return a;
    }
}

void fun(A k = 2) {}

void main()
{
    A a = 7;
    fun();
}
