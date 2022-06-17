// https://issues.dlang.org/show_bug.cgi?id=23108
/* TEST_OUTPUT:
---
fail_compilation/fail23108b.d(10): Error: undefined identifier `_xopEquals` in module `object`
fail_compilation/fail23108b.d(10): Error: undefined identifier `_xopCmp` in module `object`
---
*/
module object;

struct Interface
{
    void[] vtbl;
    int opCmp() { return 0; }
}

class TypeInfo
{
}
