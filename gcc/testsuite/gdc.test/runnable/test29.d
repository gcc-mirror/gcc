/*
COMPILE_SEPARATELY:
EXTRA_SOURCES: imports/test29a.d imports/test29b.d
PERMUTE_ARGS:
RUN_OUTPUT:
---
42
---
*/

import imports.test29a;
import imports.test29b;

extern(C) int printf(const char*, ...);

void main() {
        printf("%d\n", qwert);
}
