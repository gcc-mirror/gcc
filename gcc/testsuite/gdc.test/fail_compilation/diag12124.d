/*
TEST_OUTPUT:
---
fail_compilation/diag12124.d(14): Error: struct diag12124.S1 static opCall is hidden by constructors and can never be called
fail_compilation/diag12124.d(14):        Please use a factory method instead, or replace all constructors with static opCall.
fail_compilation/diag12124.d(20): Error: struct diag12124.S2 static opCall is hidden by constructors and can never be called
fail_compilation/diag12124.d(20):        Please use a factory method instead, or replace all constructors with static opCall.
---
*/

struct S1
{
    this(int) {}
    static S1 opCall() { assert(0); }
}

struct S2
{
    this(int) {}
    static S2 opCall()() { assert(0); }
}
