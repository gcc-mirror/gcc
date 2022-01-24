// EXTRA_SOURCES: imports/test24a.d imports/test24b.d
// EXTRA_FILES: imports/test24c.d
// PERMUTE_ARGS:
// REQUIRED_ARGS:

import imports.test24a, imports.test24b;

void main()
{
    string hi = imports.test24c.format("%s", 3);
}
