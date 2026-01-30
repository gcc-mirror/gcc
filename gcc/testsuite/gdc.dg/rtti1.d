// { dg-do compile }
// { dg-options "-fno-rtti" }
// { dg-shouldfail "expressions depend on TypeInfo" }

module object;

class Object {}
class TypeInfo {}

struct AA(K, V)
{
    this(int sz) nothrow
    {
        keyTI = typeid(K); // { dg-error "'object.TypeInfo' cannot be used with '-fno-rtti'" }
    }
    TypeInfo keyTI;
}

auto _d_aaIn(T : V[K], K, V, K2)(inout T a, auto ref scope K2 key)
{
    auto aa = *(cast(inout(AA!(K, V))*)&a); // { dg-note "instantiated from here" }
    return null;
}

int* testInExp(int key, int[int] aa)
{
    return key in aa; // { dg-note "instantiated from here" }
}
