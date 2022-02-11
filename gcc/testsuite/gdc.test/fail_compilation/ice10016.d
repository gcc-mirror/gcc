/*
TEST_OUTPUT:
---
fail_compilation/ice10016.d(33): Error: undefined identifier `unknownIdentifier`
fail_compilation/ice10016.d(47): Error: template instance `ice10016.RefCounted!(S)` error instantiating
---
*/

struct RefCounted(T)
{
    struct RefCountedStore
    {
        struct Impl
        {
            T _payload;
        }
        Impl* _store;
    }
    RefCountedStore _refCounted;

    void opAssign(typeof(this)) { }
    void opAssign(T) { }

    @property refCountedPayload()
    {
        return _refCounted._store._payload;
    }
    alias refCountedPayload this;
}

struct S
{
    int i = unknownIdentifier;
}

class C {}

class N
{
    this(C) {}
    C c() { return null; }
}

class D : N
{
    this() { super(c); }
    RefCounted!S _s;
}
