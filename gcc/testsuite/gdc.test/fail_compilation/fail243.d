// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/fail243.d(23): Deprecation: class `fail243.DepClass` is deprecated
fail_compilation/fail243.d(24): Deprecation: struct `fail243.DepStruct` is deprecated
fail_compilation/fail243.d(25): Deprecation: union `fail243.DepUnion` is deprecated
fail_compilation/fail243.d(26): Deprecation: enum `fail243.DepEnum` is deprecated
fail_compilation/fail243.d(27): Deprecation: alias `fail243.DepAlias` is deprecated
---
*/

deprecated
{
    class DepClass {}
    struct DepStruct {}
    union DepUnion {}
    enum DepEnum { A }
    alias int DepAlias;
    //typedef int DepTypedef;
}

void func(DepClass obj) {}
void func(DepStruct obj) {}
void func(DepUnion obj) {}
void func(DepEnum obj) {}
void func(DepAlias obj) {}
//void func(DepTypedef obj) {}
