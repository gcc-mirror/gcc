/* TEST_OUTPUT:
---
fail_compilation/fail17275.d(12): Error: undefined identifier `ModuleGroup`, did you mean function `moduleGroup`?
fail_compilation/fail17275.d(12): Error: `inout` on `return` means `inout` must be on a parameter as well for `inout(ModuleGroup)()`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17275

struct DSO
{
    inout(ModuleGroup) moduleGroup() { }
}

struct ThreadDSO
{
    DSO* _pdso;
    void[] _tlsRange;
}
