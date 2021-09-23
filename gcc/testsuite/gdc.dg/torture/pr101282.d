// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=101282
// { dg-do run }

void main()
{
    struct S101282
    {
        int impl;
        S101282 opUnary(string op : "-")()
        {
            return S101282(-impl);
        }
        int opCmp(int i)
        {
            return (impl < i) ? -1 : (impl > i) ? 1 : 0;
        }
    }
    auto a = S101282(120);
    a = -a;
    assert(a.impl == -120);
    a = a >= 0 ? a : -a;
    assert(a.impl == 120);
}
