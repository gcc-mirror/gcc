// PERMUTE_ARGS: -inline -g -debug -unittest
// EXTRA_FILES: imports/std15021conv.d
/*
TEST_OUTPUT:
---
hit!
---
*/

import imports.std15021conv;

class AliasDecl {}

void aliasDecl(AliasDecl ad)
{
    AliasDecl* zis = &ad;

    static if (is(typeof(to!string(*zis))))
    {
        pragma(msg, "hit!");
        to!string(*zis);
    }
}

void main() {}
