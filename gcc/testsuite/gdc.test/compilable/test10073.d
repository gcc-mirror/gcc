struct Arr(T)
{
    T[] dArr;
    alias dArr this;
    bool opEquals(Arr!T d)
    {
        foreach (idx, it; d)
        {
            if (this[idx] != it)
            {
                return false;
            }
        }
        return true;
    }
}

class Bar
{
    Arr!Foo fooQ;
}

class Foo {}    // NG

