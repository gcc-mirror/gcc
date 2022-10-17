/*
TEST_OUTPUT:
---
fail_compilation/fail273.d(10): Error: alias `fail273.b` recursive alias declaration
---
*/

// https://issues.dlang.org/show_bug.cgi?id=1054
// regression: circular aliases cause compiler stack overflow
alias a b;
alias b a;
b x;  // ICE #1
a y;  // ICE #2
