// https://issues.dlang.org/show_bug.cgi?id=16013

S s; /* Only this line has changed from above. */

struct RefCounted
{
    void opAssign(RefCounted rhs) {}
    void opAssign(S rhs) {}
    S refCountedPayload() { return S.init; }
    alias refCountedPayload this;
}

struct S { RefCounted s; }
