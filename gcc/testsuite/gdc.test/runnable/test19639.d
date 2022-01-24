enum EMPTY_STRING = ""[0..0];
enum SMALL_STRING = "a"[0..1];

void main()
{
    {
        char[64] buf = EMPTY_STRING;
        foreach (c; buf) assert(c == 0);
        buf[$-1] = 'e';
        buf = EMPTY_STRING;
        assert(buf[$-1] == 0);
    }

    {
        char[64] buf = SMALL_STRING;
        assert(buf[0] == 'a');
        foreach (c; buf[1..$]) assert(c == 0);
        buf[$-1] = 'e';
        buf = SMALL_STRING;
        assert(buf[$-1] == 0);
    }
}
