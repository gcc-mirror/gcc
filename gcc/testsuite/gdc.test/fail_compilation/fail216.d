/*
TEST_OUTPUT:
---
fail_compilation/fail216.d(16): Error: expression foo() is void and has no value
fail_compilation/fail216.d(14): Error: function fail216.bar has no return statement, but is expected to return a value of type int
fail_compilation/fail216.d(19):        called from here: bar()
---
*/

// Issue 1744 - CTFE: crash on assigning void-returning function to variable

void foo() {}

int bar()
{
    int x = foo();
}

const y = bar();
