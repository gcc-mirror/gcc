/* REQUIRED_ARGS: -preview=dip1000
 * This case winds up calling buildScopeRef() with stc having only STC.return_ set.
 */

struct PackedPtrImpl(size_t bits)
{
pure nothrow:
    this(inout(size_t)* ptr) inout @safe @nogc
    {
        origin = ptr;
    }
    size_t* origin;
}

void test()
{
    size_t* p;
    const ppi = const(PackedPtrImpl!(3))(p);
}
