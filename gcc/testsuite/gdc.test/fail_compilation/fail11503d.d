struct Data2
{
    char buffer;
}

@property const(char)[] filename(const ref Data2 d) pure nothrow
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
