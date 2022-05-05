/* TEST_OUTPUT:
---
compilable/test7172.d(14): Deprecation: `scope` as a type constraint is deprecated.  Use `scope` at the usage site.
---
*/
void main()
{
    abstract class AbstractC{}
    static assert(!__traits(compiles, { new AbstractC(); }));

    final class FinalC{}
    static assert(!__traits(compiles, { class D : FinalC{} }));

    scope class ScopeC{}
    static assert(!__traits(compiles, { auto  sc = new ScopeC(); }));
    static assert( __traits(compiles, { scope sc = new ScopeC(); }));

    synchronized class SyncC{ void f(){} }
    static assert(SyncC.f.mangleof[$-13..$] == "5SyncC1fMOFZv");

    @safe    class SCx{ void f(){} }
    @trusted class SCy{ void f(){} }
    @system  class SCz{ void f(){} }

    static assert(SCx.f.mangleof[$-12..$] == "3SCx1fMFNfZv");   // Nf: FuncAttrSafe
    static assert(SCy.f.mangleof[$-12..$] == "3SCy1fMFNeZv");   // Ne: FuncAttrTrusted
    static assert(SCz.f.mangleof[$-10..$] == "3SCz1fMFZv");     // (none)
}
