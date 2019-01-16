// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
main
+alias apiSym1
a.expensiveArgs: 1
a.expensiveTemplate: 1
-alias apiSym1
+alias apiSym3
b.expensiveArgs: 3
b.expensiveTemplate: 3
-alias apiSym3
---
*/

import imports.test13242a;

void main()
{
    pragma(msg, "main");

    cheapFunc();

    pragma(msg, "+alias apiSym1");
    alias apiSym1 = .apiSym1;
    pragma(msg, "-alias apiSym1");

    // imports.test13242a.apiSym2 is not analyzed.

    pragma(msg, "+alias apiSym3");
    alias apiSym3 = .apiSym3;
    pragma(msg, "-alias apiSym3");
}
