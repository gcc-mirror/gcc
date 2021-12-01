// https://issues.dlang.org/show_bug.cgi?id=21997

int nonPureFunc(int i)
{
    return 2 * i;
}

int mainCtfe()
{
    auto pureFunc = cast(int function(int) pure) &nonPureFunc;
    assert(pureFunc(2) == 4);

    auto baseFunc = cast(int function(int)) pureFunc;
    assert(baseFunc(3) == 6);

    /*
    Still missing delegates: https://issues.dlang.org/show_bug.cgi?id=17487

    static struct S {
        int i;
        int f(int j) { return i * j; }
    }

    S s = S(5);
    auto pureDel = cast(int delegate(int) pure) &s.f;
    assert(pureDel(3) == 15);

    auto baseDel = cast(int delegate(int)) pureDel;
    assert(baseDel(4) == 20);
    */
    return 0;
}

enum forceCTFE = mainCtfe();
