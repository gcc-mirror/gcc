/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a + 1.0F` of type `float` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a - 1.0F` of type `float` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a * 1.0F` of type `float` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a / 1.0F` of type `float` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a % 1.0F` of type `float` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a + 1.0` of type `double` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a - 1.0` of type `double` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a * 1.0` of type `double` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a / 1.0` of type `double` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a % 1.0` of type `double` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a + 1.0L` of type `real` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a - 1.0L` of type `real` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a * 1.0L` of type `real` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a / 1.0L` of type `real` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a % 1.0L` of type `real` to `byte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a + 1.0F` of type `float` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a - 1.0F` of type `float` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a * 1.0F` of type `float` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a / 1.0F` of type `float` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a % 1.0F` of type `float` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a + 1.0` of type `double` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a - 1.0` of type `double` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a * 1.0` of type `double` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a / 1.0` of type `double` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a % 1.0` of type `double` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a + 1.0L` of type `real` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a - 1.0L` of type `real` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a * 1.0L` of type `real` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a / 1.0L` of type `real` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a % 1.0L` of type `real` to `ubyte`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a + 1.0F` of type `float` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a - 1.0F` of type `float` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a * 1.0F` of type `float` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a / 1.0F` of type `float` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a % 1.0F` of type `float` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a + 1.0` of type `double` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a - 1.0` of type `double` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a * 1.0` of type `double` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a / 1.0` of type `double` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a % 1.0` of type `double` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a + 1.0L` of type `real` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a - 1.0L` of type `real` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a * 1.0L` of type `real` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a / 1.0L` of type `real` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a % 1.0L` of type `real` to `short`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a + 1.0F` of type `float` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a - 1.0F` of type `float` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a * 1.0F` of type `float` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a / 1.0F` of type `float` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)cast(int)a % 1.0F` of type `float` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a + 1.0` of type `double` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a - 1.0` of type `double` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a * 1.0` of type `double` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a / 1.0` of type `double` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)cast(int)a % 1.0` of type `double` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a + 1.0L` of type `real` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a - 1.0L` of type `real` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a * 1.0L` of type `real` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a / 1.0L` of type `real` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)cast(int)a % 1.0L` of type `real` to `ushort`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a + 1.0F` of type `float` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a - 1.0F` of type `float` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a * 1.0F` of type `float` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a / 1.0F` of type `float` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a % 1.0F` of type `float` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a + 1.0` of type `double` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a - 1.0` of type `double` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a * 1.0` of type `double` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a / 1.0` of type `double` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a % 1.0` of type `double` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a + 1.0L` of type `real` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a - 1.0L` of type `real` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a * 1.0L` of type `real` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a / 1.0L` of type `real` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a % 1.0L` of type `real` to `int`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a + 1.0F` of type `float` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a - 1.0F` of type `float` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a * 1.0F` of type `float` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a / 1.0F` of type `float` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a % 1.0F` of type `float` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a + 1.0` of type `double` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a - 1.0` of type `double` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a * 1.0` of type `double` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a / 1.0` of type `double` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a % 1.0` of type `double` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a + 1.0L` of type `real` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a - 1.0L` of type `real` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a * 1.0L` of type `real` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a / 1.0L` of type `real` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a % 1.0L` of type `real` to `uint`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a + 1.0F` of type `float` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a - 1.0F` of type `float` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a * 1.0F` of type `float` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a / 1.0F` of type `float` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a % 1.0F` of type `float` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a + 1.0` of type `double` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a - 1.0` of type `double` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a * 1.0` of type `double` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a / 1.0` of type `double` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a % 1.0` of type `double` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a + 1.0L` of type `real` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a - 1.0L` of type `real` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a * 1.0L` of type `real` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a / 1.0L` of type `real` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a % 1.0L` of type `real` to `long`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a + 1.0F` of type `float` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a - 1.0F` of type `float` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a * 1.0F` of type `float` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a / 1.0F` of type `float` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(float)a % 1.0F` of type `float` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a + 1.0` of type `double` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a - 1.0` of type `double` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a * 1.0` of type `double` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a / 1.0` of type `double` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(double)a % 1.0` of type `double` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a + 1.0L` of type `real` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a - 1.0L` of type `real` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a * 1.0L` of type `real` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a / 1.0L` of type `real` to `ulong`
fail_compilation/failexpression4.d-mixin-139(139): Error: cannot implicitly convert expression `cast(real)a % 1.0L` of type `real` to `ulong`
fail_compilation/failexpression4.d(149): Error: template instance `failexpression4.X!(integral, floating, arith)` error instantiating
fail_compilation/failexpression4.d(154):        instantiated from here: `OpReAssignCases!(TestOpAndAssign)`
---
*/
template TT(T...) { alias T TT; }

void TestOpAndAssign(Tx, Ux, ops)()
{
    foreach(T; Tx.x)
    foreach(U; Ux.x)
    foreach(op; ops.x)
    {
        T a = cast(T)1;
        U b = cast(U)1;
        mixin("a = a " ~ op[0..$-1] ~ " cast(U)1;");
    }
}

struct integral  { alias TT!(byte, ubyte, short, ushort, int, uint, long, ulong) x; }
struct floating  { alias TT!(float, double, real) x; }
struct arith     { alias TT!("+=", "-=", "*=", "/=", "%=") x; }

void OpReAssignCases(alias X)()
{
    X!(integral, floating, arith)();
}

void main()
{
    OpReAssignCases!TestOpAndAssign();
}
