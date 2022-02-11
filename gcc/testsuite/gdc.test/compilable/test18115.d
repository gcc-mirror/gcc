// https://issues.dlang.org/show_bug.cgi?id=18115

int test()
{
    if (test.stringof.length > 6 &&
        test.stringof[$-7..$] == "1234567") {}
    return 0;
}

enum a = test();
