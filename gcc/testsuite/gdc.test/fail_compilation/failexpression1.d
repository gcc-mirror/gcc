/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte += float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte -= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte *= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte /= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte %= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte += double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte -= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte *= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte /= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte %= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte += real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte -= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte *= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte /= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `byte %= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte += float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte -= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte *= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte /= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte %= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte += double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte -= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte *= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte /= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte %= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte += real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte -= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte *= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte /= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ubyte %= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short += float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short -= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short *= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short /= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short %= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short += double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short -= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short *= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short /= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short %= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short += real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short -= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short *= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short /= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `short %= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort += float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort -= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort *= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort /= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort %= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort += double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort -= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort *= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort /= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort %= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort += real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort -= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort *= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort /= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ushort %= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int += float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int -= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int *= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int /= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int %= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int += double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int -= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int *= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int /= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int %= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int += real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int -= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int *= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int /= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `int %= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint += float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint -= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint *= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint /= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint %= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint += double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint -= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint *= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint /= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint %= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint += real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint -= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint *= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint /= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `uint %= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long += float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long -= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long *= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long /= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long %= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long += double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long -= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long *= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long /= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long %= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long += real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long -= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long *= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long /= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `long %= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong += float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong -= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong *= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong /= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong %= float` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong += double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong -= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong *= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong /= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong %= double` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong += real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong -= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong *= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong /= real` is performing truncating conversion
fail_compilation/failexpression1.d-mixin-138(138): Deprecation: `ulong %= real` is performing truncating conversion
fail_compilation/failexpression1.d(148): Error: template instance `failexpression1.X!(integral, floating, arith)` error instantiating
fail_compilation/failexpression1.d(153):        instantiated from here: `OpAssignCases!(TestOpAssign)`
---
*/
template TT(T...) { alias T TT; }

void TestOpAssign(Tx, Ux, ops)()
{
    foreach(T; Tx.x)
    foreach(U; Ux.x)
    foreach(op; ops.x)
    {
        T a = cast(T)1;
        mixin("a " ~ op ~ " cast(U)1;");
    }
}

struct integral  { alias TT!(byte, ubyte, short, ushort, int, uint, long, ulong) x; }
struct floating  { alias TT!(float, double, real) x; }
struct arith     { alias TT!("+=", "-=", "*=", "/=", "%=") x; }

void OpAssignCases(alias X)()
{
    X!(integral, floating, arith)();
}

void main()
{
    OpAssignCases!TestOpAssign();
}
