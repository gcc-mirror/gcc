/* TEST_OUTPUT:
---
fail_compilation/fail17612.d(14): Error: undefined identifier `string`
fail_compilation/fail17612.d(17): Error: class object.TypeInfo missing or corrupt object.d
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17612

module object;

class Object
{
    string toString();
}

class TypeInfo {}
