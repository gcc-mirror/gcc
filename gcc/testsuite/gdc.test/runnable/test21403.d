// https://issues.dlang.org/show_bug.cgi?id=21403

/***********************************************/

int[] cat11ret3(ref int[] s)
{
    s ~= 11;
    return [3];
}

int[] doit1(int[] val)
{
    (val ~= cat11ret3(val)) ~= 7;
    return val;
}

void test1()
{
    static assert(doit1([2]) == [2, 11, 3, 7]);
    assert(doit1([2]) == [2, 11, 3, 7]);
}

/***********************************************/

char catbretc(ref char[] s)
{
    s ~= 'b';
    return 'c';
}

char[] doit2(char[] val)
{
    (val ~= catbretc(val)) ~= 'd';
    return val;
}

void test2()
{
    static assert(doit2(['a']) == ['a', 'b', 'c', 'd']);
    assert(doit2(['a']) == ['a', 'b', 'c', 'd']);
}

/***********************************************/

int cat2ret3(ref int[] s)
{
    s ~= 2;
    return 3;
}

int[] doit2(int[] val)
{
    (val ~= cat2ret3(val)) ~= 4;
    return val;
}

void test3()
{
    static assert(doit2([1]) == [1, 2, 3, 4]);
    assert(doit2([1]) == [1, 2, 3, 4]);
}

/***********************************************/

int main()
{
    test1();
    test2();
    test3();

    return 0;
}
