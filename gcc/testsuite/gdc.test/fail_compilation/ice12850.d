/*
TEST_OUTPUT:
---
fail_compilation/ice12850.d(12): Error: cannot implicitly convert expression `0` of type `int` to `string`
---
*/
alias TypeTuple(TL...) = TL;

void main()
{
    int[string] arr;
    alias staticZip = TypeTuple!(arr[0]);
}
