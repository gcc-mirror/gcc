/*
TEST_OUTPUT:
---
fail_compilation/fail11503d.d(26): Error: cannot implicitly convert expression `filename(d)` of type `const(char)[]` to `string`
fail_compilation/fail11503d.d(27): Error: cannot implicitly convert expression `filename2(& d)` of type `const(char)[]` to `string`
---
*/
struct Data2
{
    char buffer;
}

@property const(char)[] filename(const return ref Data2 d) pure nothrow
{
    return (&d.buffer)[0 .. 1];
}

@property const(char)[] filename2(const Data2* d) pure nothrow
{
    return (&d.buffer)[0 .. 1];
}

void main()
{
    Data2 d;
    string f = d.filename;
    string g = (&d).filename2;
    d.buffer = 'a';
}
