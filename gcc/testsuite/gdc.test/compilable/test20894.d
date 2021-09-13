// https://issues.dlang.org/show_bug.cgi?id=20894

mixin template MT()
{
    int   a;
    alias b = char;
    void  c() {}
}

struct S
{
    mixin MT mt;
}

void main()
{
    auto r = S();
    enum c = S();

    foo!(S.mt);
    foo!(r.mt);
    foo!(c.mt);          // OK <- ICE

    foo!(mixin("S.mt"));
    foo!(mixin("r.mt")); // OK <- ICE
    foo!(mixin("c.mt")); // OK <- ICE

    // some checks
    foo!(r.mt, c.mt);
    foo!(mixin("r.mt"), c.mt);
    foo!(r.mt, mixin("c.mt"));
    foo!(S.mt, mixin("c.mt"));
}

alias Tup(T...) = T;

void foo(A...)()
{
    static if (A.length == 2)
    {
        static assert(__traits(isSame, A[0], A[1]));
        enum members = __traits(allMembers, A[0]);
        static assert(members == __traits(allMembers, A[1]));
        static assert(members == Tup!("a", "b", "c"));
    }
}
