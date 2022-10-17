/* TEST_OUTPUT:
---
fail_compilation/aliasassign1.d(106): Error: A was read, so cannot reassign
fail_compilation/aliasassign1.d(110): Error: template instance `aliasassign1.staticMap!(Unqual, int, const(uint))` error instantiating
fail_compilation/aliasassign1.d(112): Error: static assert:  `is(TK == AliasSeq!(int, uint))` is false
---
 */

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

#line 100

template staticMap(alias F, T...)
{
    alias A = AliasSeq!();
    alias B = A;
    static foreach (t; T)
	A = AliasSeq!(A, F!t); // what's tested
    alias staticMap = A;
}

alias TK = staticMap!(Unqual, int, const uint);
//pragma(msg, TK);
static assert(is(TK == AliasSeq!(int, uint)));
