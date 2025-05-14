/*
TEST_OUTPUT:
---
fail_compilation/ice10624.d(39): Error: no operator `<` for type `Tuple`
fail_compilation/ice10624.d(13):        perhaps overload it with `int opCmp(Tuple!(Msg) other) const {}`
fail_compilation/ice10624.d(49): Error: template instance `ice10624.Variant.handler!(Tuple!(Msg))` error instantiating
fail_compilation/ice10624.d(22):        instantiated from here: `opAssign!(Tuple!(Msg))`
---
*/

struct Msg {}

struct Tuple(Specs...)
{
    Specs expand;
    alias expand this;
}

void main()
{
    Variant data;
    data = Tuple!Msg();

}

struct Variant
{
    ptrdiff_t function() fptr = &handler!(void);

    static ptrdiff_t handler(A : void)()
    {
        return 0;
    }
    static ptrdiff_t handler(A)()
    {
        A* zis;
        A* rhsPA;
        {
            return *zis < *rhsPA ? -1 : 1;
            // Tuple!(Msg) < Tuple!(Msg)
            // Tuple!(Msg).expand < Tuple!(Msg).expand
            // -> should be error
        }
        return 0;
    }

    Variant opAssign(T)(T rhs)
    {
        fptr = &handler!(T);
        return this;
    }
}
