// REQUIRED_ARGS:
shared struct S
{
    int x = 0;

    int opUnary(string s)() if (s == "++")
    {
        import core.atomic : atomicOp;
        return atomicOp!"+="(x, 1);
    }
}

shared class C
{
    int x = 0;

    int opUnary(string s)() if (s == "++")
    {
        import core.atomic : atomicOp;
        return atomicOp!"+="(x, 1);
    }
}

void main()
{
    S s;
    s++;
    shared(C) c = new C();
    c++;
}
