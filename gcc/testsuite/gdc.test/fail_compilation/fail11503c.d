struct Data
{
    char[256] buffer;
    @property const(char)[] filename() const pure nothrow
    {
        return buffer[];
    }
}

void main()
{
    Data d;
    string f = d.filename;
    d.buffer[0] = 'a';
}
