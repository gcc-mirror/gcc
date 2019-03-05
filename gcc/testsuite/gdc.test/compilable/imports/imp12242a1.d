module imports.imp12242a1;

// std.string.strip
int stripA(C)(C[] str) @safe pure
    if (is(immutable C == immutable char))
{
    return 1;
}
