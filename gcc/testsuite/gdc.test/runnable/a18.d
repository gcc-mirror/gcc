/*
COMPILE_SEPARATELY:
EXTRA_SOURCES: imports/a18a.d
PERMUTE_ARGS:
RUN_OUTPUT:
---
Test enumerator
---
*/

import imports.a18a;

extern(C) int printf(const char*, ...);

alias   IContainer!(int) icontainer_t;

int main()
{
        printf("Test enumerator\n");

        return 0;
}
