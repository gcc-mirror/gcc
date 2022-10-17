template AliasSeq(T...) { alias AliasSeq = T; }

template Unqual(T)
{
    static if (is(T U == const U))
        alias Unqual = U;
    else static if (is(T U == immutable U))
        alias Unqual = U;
    else
        alias Unqual = T;
}

template staticMap(alias F, T...)
{
    alias A = AliasSeq!();
    static foreach (t; T)
        A = AliasSeq!(A, F!t); // what's tested
    alias staticMap = A;
}

alias TK = staticMap!(Unqual, int, const uint);
//pragma(msg, TK);
static assert(is(TK == AliasSeq!(int, uint)));

/**************************************************/

template reverse(T...)
{
    alias A = AliasSeq!();
    static foreach (t; T)
        A = AliasSeq!(t, A); // what's tested
    alias reverse = A;
}

enum X2 = 3;
alias TK2 = reverse!(int, const uint, X2);
//pragma(msg, TK2);
static assert(TK2[0] == 3);
static assert(is(TK2[1] == const(uint)));
static assert(is(TK2[2] == int));

/**************************************************/

template Tp(Args...)
{
    alias Tp = AliasSeq!(int, 1, "asd", Args);
    static foreach (arg; Args)
    {
        Tp = AliasSeq!(4, Tp, "zxc", arg, Tp, 5, 4, int, Tp[0..2]);
    }
}

void fun(){}

alias a1 = Tp!(char[], fun, x => x);
static assert(
        __traits(isSame, a1, AliasSeq!(4, 4, 4, int, 1, "asd", char[], fun,
                x => x, "zxc", char[], int, 1, "asd", char[], fun, x => x,
                5, 4, int, int, 1, "zxc", fun, 4, int, 1, "asd", char[],
                fun, x => x, "zxc", char[], int, 1, "asd", char[], fun,
                x => x, 5, 4, int, int, 1, 5, 4, int, 4, int, "zxc", x => x,
                4, 4, int, 1, "asd", char[], fun, x => x, "zxc", char[],
                int, 1, "asd", char[], fun, x => x, 5, 4, int, int, 1,
                "zxc", fun, 4, int, 1, "asd", char[], fun, x => x, "zxc",
                char[], int, 1, "asd", char[], fun, x => x, 5, 4, int, int,
                1, 5, 4, int, 4, int, 5, 4, int, 4, 4)));

template Tp2(Args...)
{
    alias Tp2 = () => 1;
    static foreach (i; 0..Args.length)
        Tp2 = AliasSeq!(Tp2, Args[i]);
}

const x = 8;
static assert(
        __traits(isSame, Tp2!(2, float, x), AliasSeq!(() => 1, 2, float, x)));


enum F(int i) = i * i;

template staticMap2(alias fun, args...)
{
    alias staticMap2 = AliasSeq!();
    static foreach (i; 0 .. args.length)
        staticMap2 = AliasSeq!(fun!(args[i]), staticMap2, fun!(args[i]));
}

enum a2 = staticMap2!(F, 0, 1, 2, 3, 4);

struct Cmp(T...){}
// isSame sucks
static assert(is(Cmp!a2 == Cmp!(16, 9, 4, 1, 0, 0, 1, 4, 9, 16)));

template Tp3()
{
    alias aa1 = int;
    static foreach (t; AliasSeq!(float, char[]))
        aa1 = AliasSeq!(aa1, t);
    static assert(is(aa1 == AliasSeq!(int, float, char[])));

    alias aa2 = AliasSeq!int;
    static foreach (t; AliasSeq!(float, char[]))
        aa2 = AliasSeq!(aa2, t);
    static assert(is(aa2 == AliasSeq!(int, float, char[])));

    alias aa3 = AliasSeq!int;
    aa3 = AliasSeq!(float, char);
    static assert(is(aa3 == AliasSeq!(float, char)));
}
alias a3 = Tp3!();

template Tp4() // Uses slow path because overload
{
    alias AliasSeq(T...) = T;
    alias AliasSeq(alias f, T...) = T;

    alias aa4 = int;
    aa4 = AliasSeq!(aa4, float);
    static assert(is(aa4 == AliasSeq!(int, float)));

}
alias a4 = Tp4!();

template Tp5() // same tp overloaded, still uses fast path
{
    alias AliasSeq2(T...) = T;
    alias AliasSeq = AliasSeq2;
    alias AliasSeq = AliasSeq2;

    alias aa5 = int;
    aa5 = AliasSeq!(aa5, float);
    static assert(is(aa5 == AliasSeq!(int, float)));
}
alias a5 = Tp5!();
