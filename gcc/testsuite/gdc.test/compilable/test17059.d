mixin template impl()
{
    alias T = typeof(this);
    enum doImplement = is(T : I);

    static if (doImplement)
    {}
}

interface I {}
class A : I {mixin impl;}
