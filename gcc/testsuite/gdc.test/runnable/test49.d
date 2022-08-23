/*
COMPILE_SEPARATELY:
EXTRA_SOURCES: imports/test49a.d
PERMUTE_ARGS:
RUN_OUTPUT:
---
static this()
static ~this()
---
*/

import imports.test49a;

alias Foo!(int) foo;

void main()
{
}
