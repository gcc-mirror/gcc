/*
TEST_OUTPUT:
---
fail_compilation/onemember_overloads.d(29): Error: none of the overloads of `skipOver` are callable using argument types `()`
fail_compilation/onemember_overloads.d(25):        Candidates are: `onemember_overloads.skipOver(string)`
fail_compilation/onemember_overloads.d(18):                        `skipOver(alias pred = (a, b) => a == b)`
fail_compilation/onemember_overloads.d(20):          - Containing: `skipOver(Haystack, Needles...)(ref Haystack haystack, Needles needles)`
fail_compilation/onemember_overloads.d(21):          - Containing: `skipOver(R)(ref R r1)`
fail_compilation/onemember_overloads.d(22):          - Containing: `skipOver(R, Es...)(ref R r, Es es)`
fail_compilation/onemember_overloads.d(30): Error: template `t2` is not callable using argument types `!()()`
fail_compilation/onemember_overloads.d(33):        Candidate is: `t2(T)`
fail_compilation/onemember_overloads.d(35):          - Containing: `t2(string)`
fail_compilation/onemember_overloads.d(36):          - Containing: `t2(int[])`
fail_compilation/onemember_overloads.d(37):          - Containing: `t2(R)(R)`
---
*/

template skipOver(alias pred = (a, b) => a == b)
{
    bool skipOver(Haystack, Needles...)(ref Haystack haystack, Needles needles) => true;
    bool skipOver(R)(ref R r1) => true;
    bool skipOver(R, Es...)(ref R r, Es es) => true;
}

void skipOver(string);

void main()
{
    skipOver();
    t2();
}

template t2(T)
{
    bool t2(string);
    bool t2(int[]);
    bool t2(R)(R);
}
