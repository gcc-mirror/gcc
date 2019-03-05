// note: not actually imported, just built and linked against
// EXTRA_SOURCES: imports/extern1a.d
// PERMUTE_ARGS:

extern (C)
{
    extern int x;
}

int main()
{
    assert(x == 3);
    return 0;
}
