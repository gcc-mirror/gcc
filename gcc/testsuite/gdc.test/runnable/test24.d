// EXTRA_SOURCES: imports/test24a.d imports/test24b.d
// PERMUTE_ARGS:
// REQUIRED_ARGS:

import imports.test24a, imports.test24b;

void main()
{
    string hi = std.string.format("%s", 3);
}
