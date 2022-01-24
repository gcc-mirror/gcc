// PERMUTE_ARGS: -O
// https://issues.dlang.org/show_bug.cgi?id=20855

string exp()
{
    string s = "a = b + c * d + a;";
    foreach (i; 0 .. 9)
	s = s ~ s;
    return s;
}

int test()
{
    auto a=1, b=2, c=3, d=4;
    mixin(exp());
    return a;
}

import core.stdc.stdio;

int main()
{
    int a = test();
    printf("a = %d\n", a);
    assert(test() == 7169);
    return 0;
}
