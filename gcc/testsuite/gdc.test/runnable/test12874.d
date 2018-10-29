// EXTRA_SOURCES: imports/a12874.d
// PERMUTE_ARGS: -inline -g -O

import imports.a12874;

void main()
{
    try
    {
        int x;
        foo!(x)();
    }
    catch (Error e)
    {
        assert(e.file[$-8..$] == "a12874.d");
        assert(e.line == 7);
    }
}
