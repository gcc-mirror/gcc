module imports.imp12242b1;

// std.string.strip
int stripB(C)(C[] str) @safe pure
    if (is(immutable C == immutable char))
{
    return 1;
}
