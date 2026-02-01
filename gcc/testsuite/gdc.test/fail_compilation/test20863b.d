// must not assert/crash with mismatched declarations
/* TEST_OUTPUT:
---
fail_compilation/test20863b.d(23): Error: `Entry` isn't a template
fail_compilation/test20863b.d(23): Error: `aaGetHash` isn't a template function
fail_compilation/test20863b.d(23): Error: `aaOpEqual` isn't a template function
---
*/

module object;

class Object { }
class TypeInfo { }
class TypeInfo_AssociativeArray
{
    int Entry;
    struct aaOpEqual(K, V) { }
    struct aaGetHash(K, V) { }
}

extern(C) int main()
{
    int[int] aa;
    return 0;
}
