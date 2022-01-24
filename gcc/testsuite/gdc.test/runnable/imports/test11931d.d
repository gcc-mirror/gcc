module imports.test11931d;

template filter(alias pred)
{
    auto filter(Range)(Range r)
    {
        struct FilterResult
        {
            Range array()
            {
                return data;
            }
            Range data;
        }
        return FilterResult(r);
    }
}

struct ConnectionPoint
{
    void disconnect()
    {
        if(_f)
        {
            _f();
            _f = null;
        }
    }
    private void delegate() _f;
}

struct Signal(T, A...)
{
    ConnectionPoint add(D f)
    {
        auto rf = { _arr = _arr.filter!(a => a != f).array; };
        return ConnectionPoint();
    }

private:
    alias D = T delegate(A);
    D[] _arr;
}
