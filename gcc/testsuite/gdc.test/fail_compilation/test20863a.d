// must not assert/crash with empty declarations
/* TEST_OUTPUT:
---
fail_compilation/test20863a.d(21): Error: no property `Entry` for type `object.TypeInfo_AssociativeArray`
fail_compilation/test20863a.d(17):        class `TypeInfo_AssociativeArray` defined here
fail_compilation/test20863a.d(21): Error: no property `aaGetHash` for type `object.TypeInfo_AssociativeArray`
fail_compilation/test20863a.d(17):        class `TypeInfo_AssociativeArray` defined here
fail_compilation/test20863a.d(21): Error: no property `aaOpEqual` for type `object.TypeInfo_AssociativeArray`
fail_compilation/test20863a.d(17):        class `TypeInfo_AssociativeArray` defined here
---
*/

module object;

class Object { }
class TypeInfo { }
class TypeInfo_AssociativeArray { }

extern(C) int main()
{
    int[int] aa;
    return 0;
}
