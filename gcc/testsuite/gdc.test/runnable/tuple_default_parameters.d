void main()
{
    assert(func(1,0) == 1);
    assert(func(1) == 1);
    assert(func2() == 0);

    assert(func3() == 15);
    assert(func3(2) == 10);
    assert(func3(2, 1) == 2);

    assert(func4() == 15);

    assert(func5() == 86);
    assert(func5(1) == 84);
    assert(func5(1, 2) == 81);
    assert(func5(1, 2, 3) == 80);
    assert(func5(1, 2, 3, 4) == 74);
    assert(func5(1, 2, 3, 4, 0) == 69);
    assert(func5(1, 2, 3, 4, 0, 5) == 68);
    assert(func5(1, 2, 3, 4, 0, 5, 6) == 32);
    assert(func5(1, 2, 3, 4, 0, 5, 6, 7) == 37);
    assert(func5(1, 2, 3, 4, 0, 5, 6, 7, 8) == 42);
    assert(func5(1, 2, 3, 4, 0, 5, 6, 7, 8, 9) == 46);
}

template AliasSeq(TList...)
{
    alias AliasSeq = TList;
}

T func(T)(T value, AliasSeq!(int) params = AliasSeq!(0))
{
    return value;
}

int func2(AliasSeq!(int) params = AliasSeq!(0))
{
    return 0;
}

// https://issues.dlang.org/show_bug.cgi?id=21258
int func3(AliasSeq!(int, int) args = AliasSeq!(3, 5))
{
    return args[0] * args[1];
}

size_t func4(AliasSeq!(int, string) args = AliasSeq!(3, "hello"))
{
    return args[0] * args[1].length;
}

int func5(AliasSeq!(int, int, int) a1 = AliasSeq!(3, 5, 4),
          AliasSeq!(int, int, int) a2 = AliasSeq!(10, 5, 6),
          int a3 = 42,
          AliasSeq!(int, int, int) a4 = AliasSeq!(2, 3, 5),
          int a5 = 1,
    )
{
    return a1[0] + a1[1] + a1[2] +
        a2[0] + a2[1] + a2[2] +
        a3 +
        a4[0] + a4[1] + a4[2] +
        a5;
}
