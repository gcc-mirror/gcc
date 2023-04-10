// PERMUTE_ARGS: -preview=in
bool fun(S)(ref S[3] a) { assert(a == [42, 84, 169]); return true; }
bool fun2(S)(ref S a) { return true; }
void main()
{
    static const int[3] sa = [42, 84, 169];
    static const double sa2 = 42.42;
    static assert(fun(sa));
    static assert(fun2(sa2));
}

int f1(ref const int p)    { return p; }
int f2(ref const int[2] p) { return p[0] + p[1]; }
void test2()
{
    static immutable int[2] P = [ 0, 1 ];
    static assert(f2(P) == 1);
    immutable BigInt a, b;
    static assert(glob1.twice == b.twice);
    static assert(a.twice == b.twice);
}

struct BigInt { int[64] big; }
BigInt twice (in BigInt v) @safe pure nothrow @nogc { return v; }

immutable BigInt glob1 = BigInt.init;
