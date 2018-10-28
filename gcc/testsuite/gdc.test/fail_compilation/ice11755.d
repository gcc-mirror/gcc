// REQUIRED_ARGS: -w
/*
TEST_OUTPUT:
---
fail_compilation/ice11755.d(20): Error: '!<>=' is not defined for array comparisons
fail_compilation/ice11755.d(21): Error: use '==' for non-floating comparisons rather than floating point operator '!<>'
fail_compilation/ice11755.d(22): Error: use '!=' for non-floating comparisons rather than floating point operator '<>'
fail_compilation/ice11755.d(23): Error: '<>=' is not defined for array comparisons
fail_compilation/ice11755.d(24): Error: use '<=' for non-floating comparisons rather than floating point operator '!>'
fail_compilation/ice11755.d(25): Error: use '<' for non-floating comparisons rather than floating point operator '!>='
fail_compilation/ice11755.d(26): Error: use '>=' for non-floating comparisons rather than floating point operator '!<'
fail_compilation/ice11755.d(27): Error: use '>' for non-floating comparisons rather than floating point operator '!<='
fail_compilation/ice11755.d(28): Error: floating point operator '<>=' always returns true for non-floating comparisons
fail_compilation/ice11755.d(29): Error: floating point operator '!<>=' always returns false for non-floating comparisons
---
*/
void main()
{
    int[] a, b;
    auto r4 = a !<>= b; // TOKunord
    auto r2 = a !<>  b; // TOKue
    auto r1 = a  <>  b; // TOKlg
    auto r3 = a  <>= b; // TOKleg
    auto r8 = a !>   b; // TOKule
    auto r7 = a !>=  b; // TOKul
    auto r6 = a !<   b; // TOKuge
    auto r5 = a !<=  b; // TOKug
    assert((5 <>= 3) == 1);
    assert((5 !<>= 3) == 0);
}
