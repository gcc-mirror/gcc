
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

void TestOpAssignAssign(Tx, Ux, ops)()
{
    foreach(T; Tx.x)
    foreach(U; Ux.x)
    foreach(op; ops.x)
    {
        T a = cast(T)1;
        U b = cast(U)1;
        T r;
        mixin("r = a " ~ op ~ " cast(U)1;");
    }
}

void TestOpAssignAuto(Tx, Ux, ops)()
{
    foreach(T; Tx.x)
    foreach(U; Ux.x)
    static if (U.sizeof <= T.sizeof)
    foreach(op; ops.x)
    {
        T a = cast(T)1;
        U b = cast(U)1;
        mixin("auto r = a " ~ op ~ " cast(U)1;");
    }
}

void TestOpAndAssign(Tx, Ux, ops)()
{
    foreach(T; Tx.x)
    foreach(U; Ux.x)
    static if (U.sizeof <= T.sizeof && T.sizeof >= 4)
    foreach(op; ops.x)
    {
        T a = cast(T)1;
        U b = cast(U)1;
        mixin("a = a " ~ op[0..$-1] ~ " cast(U)1;");
    }
}

struct boolean   { alias TT!(bool) x; }
struct integral  { alias TT!(byte, ubyte, short, ushort, int, uint, long, ulong) x; }
struct floating  { alias TT!(float, double, real) x; }
struct imaginary { alias TT!(ifloat, idouble, ireal) x; }
struct complex   { alias TT!(cfloat, cdouble, creal) x; }

struct all       { alias TT!("+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", ">>>=") x; }
struct arith     { alias TT!("+=", "-=", "*=", "/=", "%=") x; }
struct bitwise   { alias TT!("&=", "|=", "^=") x; }
struct shift     { alias TT!("<<=", ">>=", ">>>=") x; }
struct addsub    { alias TT!("+=", "-=") x; }
struct muldivmod { alias TT!("*=", "/=", "%=") x; }
struct nomod     { alias TT!("+=", "-=", "*=", "/=") x; }

void OpAssignCases(alias X)()
{
    X!(boolean, boolean, bitwise)();

    X!(integral, boolean, all)();
    X!(integral, integral, all)();
    X!(integral, floating, arith)();

    X!(floating, boolean, arith)();
    X!(floating, integral, arith)();
    X!(floating, floating, arith)();

    X!(imaginary, boolean, muldivmod)();
    X!(imaginary, integral, muldivmod)();
    X!(imaginary, floating, muldivmod)();
    X!(imaginary, imaginary, addsub)();

    X!(complex, boolean, arith)();
    X!(complex, integral, arith)();
    X!(complex, floating, arith)();
    X!(complex, imaginary, arith)();
    X!(complex, complex, nomod)();
}

void OpReAssignCases(alias X)()
{
    X!(boolean, boolean, bitwise)();

    X!(integral, boolean, all)();
    X!(integral, integral, all)();

    X!(floating, boolean, arith)();
    X!(floating, integral, arith)();
    X!(floating, floating, arith)();

    X!(imaginary, boolean, muldivmod)();
    X!(imaginary, integral, muldivmod)();
    X!(imaginary, floating, muldivmod)();
    X!(imaginary, imaginary, addsub)();

    X!(complex, boolean, arith)();
    X!(complex, integral, arith)();
    X!(complex, floating, arith)();
    X!(complex, imaginary, arith)();
    X!(complex, complex, nomod)();
}

void main()
{
    OpAssignCases!TestOpAssign();
    OpAssignCases!TestOpAssignAssign(); // was once disabled due to bug 7436
    OpAssignCases!TestOpAssignAuto(); // 5181
    OpReAssignCases!TestOpAndAssign();
}
