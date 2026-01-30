// { dg-do compile }
// { dg-options "-fno-rtti" }
// { dg-shouldfail "expressions depend on TypeInfo" }

module object;

class Object {}
class TypeInfo {}
class TypeInfo_Array : TypeInfo {}

struct AA(K, V)
{
    this(int sz) nothrow
    {
        keyTI = typeid(K); // { dg-error "'object.TypeInfo' cannot be used with '-fno-rtti'" }
    }
    TypeInfo keyTI;
}

bool _d_aaEqual(K, V)(scope const V[K] a1, scope const V[K] a2)
{
    auto aa1 = *(cast(const(AA!(K, V))*)&a1); // { dg-note "instantiated from here" }
    return false;
}

bool testAAEqual(int[immutable(char)[]] aa1, int[immutable(char)[]] aa2)
{
    return aa1 == aa2; // { dg-note "instantiated from here" }
}
