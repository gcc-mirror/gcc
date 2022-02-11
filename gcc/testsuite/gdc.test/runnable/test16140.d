// https://issues.dlang.org/show_bug.cgi?id=16140

int fun()
{
    static int count = 0;
    if (count == 3)
    {
        count = 0;
        return 0;
    }
    ++count;
    return count;
}

void main()
{
    uint[] res;
    while(auto value = fun())
        res ~= value;
    assert(res == [1, 2, 3]);

    res.length = 0;
    while(uint value = fun())
        res ~= value;
    assert(res == [1, 2, 3]);

    res.length = 0;
    while(const value = fun())
        res ~= value;
    assert(res == [1, 2, 3]);
}

