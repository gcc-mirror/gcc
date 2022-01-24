// https://issues.dlang.org/show_bug.cgi?id=21096

/*
TEST_OUTPUT:
---
fail_compilation/test21096.d(13): Error: identifier or new keyword expected following `(...)`.
fail_compilation/test21096.d(13): Error: found `.` when expecting `]`
fail_compilation/test21096.d(13): Error: no identifier for declarator `char`
fail_compilation/test21096.d(13): Error: declaration expected, not `]`
---
*/

char[(void*).];
