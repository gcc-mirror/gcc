// { dg-do compile }
// { dg-options "-fno-rtti" }
// { dg-shouldfail "expressions depend on TypeInfo" }

int* testInExp(int key, int[int] aa)
{
    return key in aa; // { dg-error "requires .object.TypeInfo. and cannot be used with .-fno-rtti." }
}

bool testAAEqual(int[string] aa1, int[string] aa2)
{
    return aa1 == aa2; // { dg-error "requires .object.TypeInfo. and cannot be used with .-fno-rtti." }
}
