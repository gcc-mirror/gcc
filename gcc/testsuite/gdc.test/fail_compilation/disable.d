/*
TEST_OUTPUT:
---
fail_compilation/disable.d(50): Error: function disable.DisabledOpAssign.opAssign is not callable because it is annotated with @disable
fail_compilation/disable.d(53): Error: function disable.DisabledPostblit.opAssign is not callable because it is annotated with @disable
fail_compilation/disable.d(56): Error: function disable.HasDtor.opAssign is not callable because it is annotated with @disable
fail_compilation/disable.d(60): Error: generated function disable.Nested!(DisabledOpAssign).Nested.opAssign is not callable because it is annotated with @disable
fail_compilation/disable.d(63): Error: generated function disable.Nested!(DisabledPostblit).Nested.opAssign is not callable because it is annotated with @disable
fail_compilation/disable.d(66): Error: generated function disable.Nested!(HasDtor).Nested.opAssign is not callable because it is annotated with @disable
fail_compilation/disable.d(70): Error: generated function disable.NestedDtor!(DisabledOpAssign).NestedDtor.opAssign is not callable because it is annotated with @disable
fail_compilation/disable.d(73): Error: generated function disable.NestedDtor!(DisabledPostblit).NestedDtor.opAssign is not callable because it is annotated with @disable
fail_compilation/disable.d(76): Error: generated function disable.NestedDtor!(HasDtor).NestedDtor.opAssign is not callable because it is annotated with @disable
---
 */
struct DisabledOpAssign {
    int x;
    @disable void opAssign(const DisabledOpAssign);
}

struct DisabledPostblit {
    int x;
    @disable void opAssign(const DisabledPostblit);
    // Doesn't require opAssign
    @disable this(this);
}

struct HasDtor {
    int x;
    @disable void opAssign(const HasDtor);
    ~this() {} // Makes opAssign mandatory
}


struct Nested (T)
{
    T b;
}

struct NestedDtor (T)
{
    T b;

    // Requires an identity opAssign
    ~this() {}
}

void main ()
{
    DisabledOpAssign o;
    o = DisabledOpAssign();

    DisabledPostblit p;
    p = DisabledPostblit();

    HasDtor d;
    d = HasDtor();


    Nested!(DisabledOpAssign) no;
    no = Nested!(DisabledOpAssign)();

    Nested!(DisabledPostblit) np;
    np = Nested!(DisabledPostblit)();

    Nested!(HasDtor) nd;
    nd = Nested!(HasDtor)();


    NestedDtor!(DisabledOpAssign) ndo;
    ndo = NestedDtor!(DisabledOpAssign)();

    NestedDtor!(DisabledPostblit) ndp;
    ndp = NestedDtor!(DisabledPostblit)();

    NestedDtor!(HasDtor) ndd;
    ndd = NestedDtor!(HasDtor)();
}
