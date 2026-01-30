/*
TEST_OUTPUT:
---
fail_compilation/nonvoid_return.d(15): Error: cannot return non-void from `void` function
fail_compilation/nonvoid_return.d(18): Error: cannot return non-void from `void` function
fail_compilation/nonvoid_return.d(29): Error: cannot return non-void from `void` function
fail_compilation/nonvoid_return.d(32): Error: undefined identifier `NONEXISTENT`
---
*/



void main()
{
    return 10;
}

void fs() => "a";

class MyClass
{
}

MyClass[char[]] myarray;

void fn()
{
    foreach (MyClass mc; myarray)
        return mc;
}

auto err() { NONEXISTENT++; }

// Because `err` contains an error, it fails to infer void and gets an error return type
// Don't print the 'cannot return non-void' error in this case

void fe() => err;
