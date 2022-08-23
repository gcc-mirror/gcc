// https://issues.dlang.org/show_bug.cgi?id=18216

struct Node
{
    mixin Type!();
    Pointer left;
}

mixin template Type()
{
    alias Base = typeof(this);

    static struct Proxy
    {
        struct Node
        {
            Base m_payload;
        }
        static immutable default_value = Base.init; // just remove this will work
    }

    alias pNode = shared(Proxy.Node)*;

    static struct Pointer
    {
        Base*   _ptr;
        auto ptr()
        {
            return cast(pNode) _ptr;
        }

        void opAssign(ref Pointer other) {} // just remove this will work

        alias getThis this; // just remove this will work
        ref auto getThis() return
        {
            return ptr.m_payload;
        }
    }
}
