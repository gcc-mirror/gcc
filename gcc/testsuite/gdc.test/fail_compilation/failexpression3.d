/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte += float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte -= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte *= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte /= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte %= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte += double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte -= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte *= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte /= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte %= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte += real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte -= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte *= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte /= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `byte %= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte += float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte -= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte *= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte /= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte %= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte += double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte -= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte *= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte /= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte %= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte += real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte -= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte *= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte /= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ubyte %= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short += float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short -= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short *= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short /= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short %= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short += double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short -= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short *= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short /= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short %= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short += real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short -= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short *= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short /= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `short %= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort += float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort -= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort *= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort /= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort %= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort += double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort -= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort *= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort /= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort %= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort += real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort -= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort *= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort /= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ushort %= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int += float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int -= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int *= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int /= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int %= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int += double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int -= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int *= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int /= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int %= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int += real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int -= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int *= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int /= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `int %= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint += float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint -= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint *= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint /= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint %= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint += double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint -= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint *= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint /= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint %= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint += real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint -= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint *= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint /= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `uint %= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long += float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long -= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long *= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long /= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long %= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long += double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long -= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long *= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long /= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long %= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long += real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long -= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long *= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long /= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `long %= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong += float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong -= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong *= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong /= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong %= float` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong += double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong -= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong *= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong /= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong %= double` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong += real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong -= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong *= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong /= real` is performing truncating conversion
fail_compilation/failexpression3.d-mixin-139(139): Deprecation: `ulong %= real` is performing truncating conversion
fail_compilation/failexpression3.d(149): Error: template instance `failexpression3.X!(integral, floating, arith)` error instantiating
fail_compilation/failexpression3.d(154):        instantiated from here: `OpAssignCases!(TestOpAssignAuto)`
---
*/
template TT(T...) { alias T TT; }

void TestOpAssignAuto(Tx, Ux, ops)()
{
    foreach(T; Tx.x)
    foreach(U; Ux.x)
    foreach(op; ops.x)
    {
        T a = cast(T)1;
        U b = cast(U)1;
        mixin("auto r = a " ~ op ~ " cast(U)1;");
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
    OpAssignCases!TestOpAssignAuto(); // https://issues.dlang.org/show_bug.cgi?id=5181
}
