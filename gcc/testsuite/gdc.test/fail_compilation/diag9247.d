/*
TEST_OUTPUT:
---
fail_compilation/diag9247.d(11): Error: functions cannot return opaque type `S` by value
fail_compilation/diag9247.d(12): Error: functions cannot return opaque type `S` by value
---
*/

struct S;

S foo();
S function() bar;
