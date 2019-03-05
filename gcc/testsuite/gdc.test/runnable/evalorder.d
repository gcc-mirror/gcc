extern(C) int printf(const char*, ...);

void test14040()
{
    uint[] values = [0, 1, 2, 3, 4, 5, 6, 7];
    uint offset = 0;

    auto a1 = values[offset .. offset += 2];
    if (a1 != [0, 1] || offset != 2)
        assert(0);

    uint[] fun()
    {
        offset += 2;
        return values;
    }
    auto a2 = fun()[offset .. offset += 2];
    if (a2 != [4, 5] || offset != 6)
        assert(0);

    // Also test an offset of type size_t such that it is used
    // directly without any implicit conversion in the slice expression.
    size_t offset_szt = 0;
    auto a3 = values[offset_szt .. offset_szt += 2];
    if (a3 != [0, 1] || offset_szt != 2)
        assert(0);
}

/******************************************/

int add8ret3(T)(ref T s)
{
    s += 8;
    return 3;
}

int mul11ret3(T)(ref T s)
{
    s *= 11;
    return 3;
}

void add()
{
    static int test1(int val) { val += add8ret3(val); return val; }
    assert(test1(1) == (1 + 8 + 3));
    static assert(test1(1) == (1 + 8 + 3));

    static int test2(int val) { val = val + add8ret3(val); return val; }
    // FIXME: assert(test2(1) == (1 + 3));
    static assert(test2(1) == (1 + 3));

    static int test3(int val) { (val += 7) += mul11ret3(val); return val; }
    assert(test3(2) == (((2+7)*11) + 3));
    static assert(test3(2) == (((2+7)*11) + 3));
}

void min()
{
    static int test1(int val) { val -= add8ret3(val); return val; }
    assert(test1(1) == (1 + 8 - 3));
    static assert(test1(1) == (1 + 8 - 3));

    static int test2(int val) { val = val - add8ret3(val); return val; }
    // FIXME: assert(test2(1) == (1 - 3));
    static assert(test2(1) == (1 - 3));

    static int test3(int val) { (val -= 7) -= mul11ret3(val); return val; }
    assert(test3(2) == (((2-7)*11) - 3));
    static assert(test3(2) == (((2-7)*11) - 3));
}

void mul()
{
    static int test1(int val) { val *= add8ret3(val); return val; }
    assert(test1(7) == ((7 + 8) * 3));
    static assert(test1(7) == ((7 + 8) * 3));

    static int test2(int val) { val = val * add8ret3(val); return val; }
    // FIXME: assert(test2(7) == (7 * 3));
    static assert(test2(7) == (7 * 3));

    static int test3(int val) { (val *= 7) *= add8ret3(val); return val; }
    assert(test3(2) == (((2*7)+8) * 3));
    static assert(test3(2) == (((2*7)+8) * 3));
}

void xor()
{
    static int test1(int val) { val ^= add8ret3(val); return val; }
    assert(test1(1) == ((1 + 8) ^ 3));
    static assert(test1(1) == ((1 + 8) ^ 3));

    static int test2(int val) { val = val ^ add8ret3(val); return val; }
    // FIXME: assert(test2(1) == (1 ^ 3));
    static assert(test2(1) == (1 ^ 3));

    static int test3(int val) { (val ^= 7) ^= add8ret3(val); return val; }
    assert(test3(2) == (((2^7)+8) ^ 3));
    static assert(test3(2) == (((2^7)+8) ^ 3));
}

void addptr()
{
    static int* test1(int* val) { val += add8ret3(val); return val; }
    assert(test1(cast(int*)4) == ((cast(int*)4) + 8 + 3));

    static int* test2(int* val) { val = val + add8ret3(val); return val; }
    // FIXME: assert(test2(cast(int*)4) == ((cast(int*)4) + 3));

    static int* test3(int* val) { (val += 7) += add8ret3(val); return val; }
    assert(test3(cast(int*)16) == ((cast(int*)16) + 7 + 8 + 3));
}

void lhsCast()
{
    static byte test(byte val)
    {
        // lhs type `byte`, rhs type `int` =>
        // rewritten to `cast(int)(cast(int)val += 10) -= mul11ret3(val)`
        (val += 10) -= mul11ret3(val);
        return val;
    }

    assert(test(1) == ((1 + 10) * 11 - 3));
    static assert(test(1) == ((1 + 10) * 11 - 3));
}

void shr()
{
    static ubyte test(ubyte val)
    {
        // lhs type `ubyte`, rhs type `int` =>
        // rewritten to `cast(int)val >>= 1`
        // we still want a logical (unsigned) right-shift though
        val >>= 1;
        return val;
    }

    assert(test(0x80) == 0x40);
    static assert(test(0x80) == 0x40);
}

void ldc_github_1617()
{
    add();
    min();
    mul();
    xor();
    addptr();
    lhsCast();
    shr();
}

/******************************************/

int main()
{
    test14040();
    ldc_github_1617();

    printf("Success\n");
    return 0;
}
