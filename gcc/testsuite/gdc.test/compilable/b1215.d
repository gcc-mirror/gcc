// PERMUTE_ARGS:
// REQUIRED_ARGS: -o-

struct A(Args...)
{
    enum i = 1;

    // base use case.
    Args[0].T mBase;
    static assert(is(typeof(mBase) == B.T));
    // chained types
    Args[0].T.TT mChain;
    static assert(is(typeof(mChain) == B.T.TT));
    // chained packs
    Args[1+1].FArgs[0] mChainPack;
    static assert(is(typeof(mChainPack) == B));
    // expr
    enum mExpr = Args[1].i;
    static assert(mExpr == B.i);
    // Nested + index eval
    Args[Args[0].i2].T mNested;
    static assert(is(typeof(mNested) == B.T));
    // index with constexpr
    Args[i].T mCEIndex;
    static assert(is(typeof(mCEIndex) == B.T));
    // Nested + index with constexpr
    Args[Args[i].i2].T mNestedCE;
    static assert(is(typeof(mNestedCE) == B.T));

    // alias, base use case
    alias UBase = Args[0].T;
    static assert(is(UBase == B.T));
    // alias, chained types
    alias UChain = Args[0].T.TT;
    static assert(is(UChain == B.T.TT));
    // alias, chained packs
    alias UChainPack = Args[1+1].FArgs[0];
    static assert(is(UChainPack == B));
    // alias, expr
    alias uExpr = Args[1].i;
    static assert(uExpr == B.i);
    // alias, Nested + index eval
    alias UNested = Args[Args[0].i2].T;
    static assert(is(UNested == B.T));
    // alias, index with constexpr
    alias UCEIndex = Args[i].T;
    static assert(is(UCEIndex == B.T));
    // alias, Nested + index with constexpr
    alias UNextedCE = Args[Args[i].i2].T;
    static assert(is(UNextedCE == B.T));
}

struct B
{
    struct T
    {
        struct TT
        {
        }
    }
    enum i = 6;
    enum i2 = 0;
}

struct C(Args...)
{
    alias FArgs = Args;
}

alias Z = A!(B,B,C!(B,B));

/***************************************************/
// 14889

struct A14889(alias Exc)
{
    alias ExceptionType = Exc;
}
alias TT14889(Args...) = Args;

alias X14889a = TT14889!(A14889!Throwable());
alias Y14889a = X14889a[0].ExceptionType;

alias X14889b = TT14889!(A14889!Throwable);
alias Y14889b = X14889b[0].ExceptionType;

/***************************************************/
// 14889

alias TypeTuple14900(T...) = T;

struct S14900
{
    alias T = int;
    alias U = TypeTuple14900!(long,string);
}

alias Types14900 = TypeTuple14900!(S14900, S14900);

Types14900[0].T a14900;     // Types[0] == S, then typeof(a) == S.T == int
Types14900[0].U[1] b14900;  // Types[0].U == S.U, then typeof(b) == S.U[1] == string

void test14900()
{
    Types14900[0].T a;      // Types[0] == S, then typeof(a) == S.T == int
    Types14900[0].U[1] b;   // Types[0].U == S.U, then typeof(b) == S.U[1] == string
}

/***************************************************/
// 14911

void test14911()
{
    struct S {}

    int* buf1 = new int[2].ptr; // OK
    S* buf2 = (new S[2]).ptr;   // OK
    S* buf3 = new S[2].ptr;     // OK <- broken
}

/***************************************************/
// 14986

alias Id14986(alias a) = a;

struct Foo14986
{
    int tsize;
}
struct Bar14986
{
    enum Foo14986[] arr = [Foo14986()];
}

Bar14986 test14986()
{
    Foo14986[] types;
    auto a1 = new void[types[0].tsize];                 // TypeIdentifier::toExpression
    auto a2 = new void[Id14986!types[0].tsize];         // TypeInstance::toExpression

    Bar14986 bar;
    auto a3 = Id14986!(typeof(bar).arr[0].tsize);       // TypeTypeof::resolve
    auto a4 = Id14986!(typeof(return).arr[0].tsize);    // TypeReturn::resolve

    return Bar14986();
}
