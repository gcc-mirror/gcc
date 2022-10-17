// https://issues.dlang.org/show_bug.cgi?id=6795
void check6795()
{
    enum int[] array = [0];
    // PostExp
    assert(array[0]++ == 0);
    assert(array[0]-- == 0);
    // PreExp
    assert(++array[0] == 1);
    assert(--array[0] == -1);
    // BinAssignExp
    assert((array[0] += 3) == 3);
}

// https://issues.dlang.org/show_bug.cgi?id=21312
void check21312()
{
    auto p = &[123][0];
    assert(*p == 123);
}

void main()
{
    check6795();
    check21312();
}
