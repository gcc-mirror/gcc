// REQUIRED_ARGS: -debug -O -release
// https://issues.dlang.org/show_bug.cgi?id=22277

bool secret = false;

void free(immutable void* x) pure nothrow
{
    debug secret = true;
}

void main()
{
    free(null);
    if (!secret)
        assert(0);
}
