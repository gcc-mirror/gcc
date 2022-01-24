/* REQUIRED_ARGS: -O -fPIC
 * DISABLED: win32 win64
 */
// https://issues.dlang.org/show_bug.cgi?id=20466

extern (C++) final class Parameter
{
    ulong storageClass;
    void* type;
}

extern (C++) final class IfStatement
{
    Parameter prm;
}

extern (C++) final class Visitor
{
    void visit(IfStatement s)
    {
        if (Parameter p = s.prm)
        {
            ulong stc = p.storageClass;
            if (!p.type && !stc)
                stc = 1L << 8;
            assert(!(stc & (1L << 34)));
        }
    }
}

int main()
{
    auto p = new Parameter;
    p.storageClass = 1L << 2;
    auto s = new IfStatement;
    s.prm = p;
    auto v = new Visitor;
    v.visit(s);
    return 0;
}
