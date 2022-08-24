/* https://issues.dlang.org/show_bug.cgi?id=23181
TEST_OUTPUT:
---
$p:druntime/import/core/lifetime.d$($n$): Error: struct `fail23181.fail23181.NoPostblit` is not copyable because it has a disabled postblit
$p:druntime/import/core/internal/array/construction.d$($n$): Error: template instance `core.lifetime.copyEmplace!(NoPostblit, NoPostblit)` error instantiating
fail_compilation/fail23181.d(15):        instantiated from here: `_d_arraysetctor!(NoPostblit[], NoPostblit)`
---
*/
void fail23181()
{
    struct NoPostblit
    {
        @disable this(this);
    }
    NoPostblit[4] noblit23181 = NoPostblit();
}
