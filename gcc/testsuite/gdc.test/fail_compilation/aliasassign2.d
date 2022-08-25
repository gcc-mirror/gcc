/* TEST_OUTPUT:
---
fail_compilation/aliasassign2.d(16): Error: `alias aa1 = aa1;` cannot alias itself, use a qualified name to create an overload set
fail_compilation/aliasassign2.d(19): Error: template instance `aliasassign2.Tp1!()` error instantiating
fail_compilation/aliasassign2.d(24): Error: undefined identifier `unknown`
fail_compilation/aliasassign2.d(26): Error: template instance `aliasassign2.Tp2!()` error instantiating
fail_compilation/aliasassign2.d(31): Error: template instance `AliasSeqX!(aa3, 1)` template `AliasSeqX` is not defined, did you mean AliasSeq(T...)?
fail_compilation/aliasassign2.d(33): Error: template instance `aliasassign2.Tp3!()` error instantiating
---
*/

alias AliasSeq(T...) = T;

template Tp1()
{
    alias aa1 = aa1;
    aa1 = AliasSeq!(aa1, float);
}
alias a1 = Tp1!();

template Tp2()
{
    alias aa2 = AliasSeq!();
    aa2 = AliasSeq!(aa2, unknown);
}
alias a2 = Tp2!();

template Tp3()
{
    alias aa3 = AliasSeq!();
    aa3 = AliasSeqX!(aa3, 1);
}
alias a3 = Tp3!();
