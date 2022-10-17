/*
TEST_OUTPUT:
---
fail_compilation/fail11503c.d(19): Error: cannot implicitly convert expression `d.filename()` of type `const(char)[]` to `string`
---
*/
struct Data
{
    char[256] buffer;
    @property const(char)[] filename() const pure nothrow return
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
