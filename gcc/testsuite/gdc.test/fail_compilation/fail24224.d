/+
TEST_OUTPUT:
---
fail_compilation/fail24224.d(19): Error: struct / class type expected as argument to __traits(initSymbol) instead of `ES`
fail_compilation/fail24224.d(20): Error: struct / class type expected as argument to __traits(initSymbol) instead of `EU`
fail_compilation/fail24224.d(21): Error: struct / class type expected as argument to __traits(initSymbol) instead of `EC`
---
+/
struct S {}
union U {}
class C {}

enum ES : S { a = S.init }
enum EU : U { a = U.init }
enum EC : C { a = C.init }

void test()
{
    auto init1 = __traits(initSymbol, ES);
    auto init2 = __traits(initSymbol, EU);
    auto init3 = __traits(initSymbol, EC);
}
