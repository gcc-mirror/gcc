// EXTRA_SOURCES: imports/test50a.d
// PERMUTE_ARGS:

import imports.test50a;

class Bar : Foo {
        alias typeof(Foo.tupleof) Bleh;
}


