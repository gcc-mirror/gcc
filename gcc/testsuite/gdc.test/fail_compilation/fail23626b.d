/* TEST_OUTPUT:
---
fail_compilation/fail23626b.d(26): Error: `fail23626b.AmbigOpApply.opApply` called with argument types `(int delegate(int i) pure nothrow @nogc @system)` matches both:
fail_compilation/fail23626b.d(12):     `fail23626b.AmbigOpApply.opApply(int delegate(int) dg)`
and:
fail_compilation/fail23626b.d(17):     `fail23626b.AmbigOpApply.opApply(int delegate(int) dg)`
---
*/

struct AmbigOpApply
{
    int opApply(int delegate(int) dg)
    {
        return 0;
    }

    int opApply(int delegate(int) dg) @system
    {
        return 0;
    }
}

void ambigOpApply() @system
{
    AmbigOpApply sa;
    foreach (int i; sa) { }
}
