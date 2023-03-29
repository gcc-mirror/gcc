struct MyTuple {
    string s;
}

inout(string) myfront(inout(string)[] a)
{
    return a[0];
}

MyTuple[] myarray(MyZip r)
{
    MyTuple[] result;
    foreach (e; r)
        result ~= e;
    return result;
}

struct MyZip
{
    bool empty = false;
    MyTuple front()
    {
        return MyTuple([""].myfront);
    }
    void popFront()
    {
        empty = true;
    }
}

static foreach(t; MyZip().myarray) {}
