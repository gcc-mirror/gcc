module imports.link11069z;
struct Matrix(T, uint _M)
{
    int opCmp()(auto ref in Matrix b) const
    {
        return 0;
    }

    pure auto opDispatch(string s)()
    {
        enum L = s.length;
        Matrix!(T, L) ret;
        return ret;
    }

    pure Matrix normalized()
    {
        return Matrix();
    }
}

alias Matrix!(float, 2) Vector2;
