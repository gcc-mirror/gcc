/*
EXTRA_SOURCES: imports/test10a.d
RUN_OUTPUT:
---
it is 32
---
*/

import imports.test10a;

extern(C) int printf(const char*, ...);

int main()
{
    imports.test10a.init();
    printf("it is %d\n", it[0]);
    assert(it[0] == 32);
    return 0;
}
