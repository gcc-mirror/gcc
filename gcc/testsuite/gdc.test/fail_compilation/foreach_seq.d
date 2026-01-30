/*
TEST_OUTPUT:
---
fail_compilation/foreach_seq.d(25): Error: only one (element) or two (index, element) arguments allowed for sequence `foreach`, not 3
fail_compilation/foreach_seq.d(26): Error: invalid storage class `ref` for index `i`
fail_compilation/foreach_seq.d(27): Error: foreach: index cannot be of non-integral type `void`
fail_compilation/foreach_seq.d(28): Error: index type `bool` cannot cover index range 0..3
fail_compilation/foreach_seq.d(29): Error: `foreach` loop variable cannot be both `enum` and `alias`
fail_compilation/foreach_seq.d(30): Error: constant value `1` cannot be `ref`
fail_compilation/foreach_seq.d(31): Error: invalid storage class `enum` for index `i`
fail_compilation/foreach_seq.d(33): Error: invalid storage class `ref` for element `e`
fail_compilation/foreach_seq.d(34): Error: symbol `object` cannot be `ref`
fail_compilation/foreach_seq.d(35): Error: cannot specify element type for symbol `e`
fail_compilation/foreach_seq.d(36): Error: cannot specify element type for symbol `object`
fail_compilation/foreach_seq.d(37): Error: invalid storage class `enum` for element `e`
---
*/

// test semantic errors on foreach parameters
void main()
{
    alias AliasSeq(A...) = A;
    alias seq = AliasSeq!(1, 2, 3);

    foreach (a, b, c; seq) {}
    foreach (ref i, e; seq) {}
    foreach (void i, e; seq) {}
    foreach (bool i, e; seq) {}
    foreach (enum alias e; seq) {}
    foreach (ref enum e; seq) {}
    foreach (ref enum i, e; seq) {}

    foreach (ref e; AliasSeq!int) {}
    foreach (ref e; AliasSeq!object) {}
    foreach (int e; AliasSeq!int) {}
    foreach (int e; AliasSeq!object) {}
    foreach (enum e; AliasSeq!int) {}
}
