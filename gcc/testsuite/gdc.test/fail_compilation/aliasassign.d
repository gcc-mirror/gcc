/* TEST_OUTPUT:
---
fail_compilation/aliasassign.d(13): Error: `B` must have same parent `Swap!(int, string)` as alias `B`
fail_compilation/aliasassign.d(14): Error: `A` must have same parent `Swap!(int, string)` as alias `A`
fail_compilation/aliasassign.d(21): Error: template instance `aliasassign.Swap!(int, string)` error instantiating
fail_compilation/aliasassign.d(21):        while evaluating: `static assert(Swap!(int, string))`
---
*/

template Swap (alias A, alias B)
{
    alias C = A;
    B = A;
    A = B;
    enum Swap = true;
}

alias A = int;
alias B = string;

static assert(Swap!(A, B));
