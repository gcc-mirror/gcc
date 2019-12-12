// PERMUTE_ARGS:
// EXTRA_SOURCES: imports/a15760.d

module ice15760;

import imports.a15760 : Foo;

struct Bar
{
    __gshared Foo foo;
}
