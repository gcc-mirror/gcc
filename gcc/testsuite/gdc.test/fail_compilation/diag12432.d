/*
TEST_OUTPUT:
---
fail_compilation/diag12432.d(55): Error: cannot infer argument types, expected 1 argument, not 2
fail_compilation/diag12432.d(56): Error: cannot infer argument types, expected 2 arguments, not 3
fail_compilation/diag12432.d(57): Error: cannot infer argument types, expected 1 argument, not 2
fail_compilation/diag12432.d(58): Error: cannot infer argument types, expected 1 argument, not 2
fail_compilation/diag12432.d(59): Error: cannot infer argument types, expected 2 arguments, not 3
fail_compilation/diag12432.d(60): Error: cannot infer argument types, expected 2 arguments, not 3
---
*/

struct R1
{
    @property int front() { return 0; }
    enum bool empty = false;
    void popFront() { }
}

struct Tuple(T...)
{
    T t;
    alias t this;
}

struct R2
{
    @property Tuple!(int, float) front() { return typeof(return).init; }
    enum bool empty = false;
    void popFront() { }
}

struct OpApply1Func
{
    int opApply(int function(int)) { return 0; }
}

struct OpApply1Deleg
{
    int opApply(int delegate(int)) { return 0; }
}

struct OpApply2Func
{
    int opApply(int function(int, float)) { return 0; }
}

struct OpApply2Deleg
{
    int opApply(int delegate(int, float)) { return 0; }
}

void main()
{
    foreach (a, b; R1()) { }
    foreach (a, b, c; R2()) { }
    foreach (a, b; OpApply1Func()) { }
    foreach (a, b; OpApply1Deleg()) { }
    foreach (a, b, c; OpApply2Func()) { }
    foreach (a, b, c; OpApply2Deleg()) { }
}
