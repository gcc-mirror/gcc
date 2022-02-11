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

