module imports.test11931d;

import std.array;
import std.algorithm;

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
    D _arr[];
}
