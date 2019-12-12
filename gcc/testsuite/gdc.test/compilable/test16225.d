// REQUIRED_ARGS: -O -m64
// PERMUTE_ARGS:

// https://issues.dlang.org/show_bug.cgi?id=16225

struct C
{
    hash_t foo( )
    {
        int y;
        return ((cast(ubyte*)&y)[1]);
    }
}

