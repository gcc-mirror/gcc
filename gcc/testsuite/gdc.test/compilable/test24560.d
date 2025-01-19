// https://issues.dlang.org/show_bug.cgi?id=24560

class C { }
struct S
{
    static void fun(C heur = new C) { }
}

void main()
{
    S.fun();
}
