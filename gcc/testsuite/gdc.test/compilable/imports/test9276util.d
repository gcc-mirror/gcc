module imports.test9276util;

string _dgliteral(T...)()
{
    foreach (t; T)
        return t.stringof;
    assert(0);
}
template DownCastMethods(T...)
{
    enum x = _dgliteral!T;
}
