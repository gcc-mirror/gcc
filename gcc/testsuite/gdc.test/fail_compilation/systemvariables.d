/*
REQUIRED_ARGS: -preview=systemVariables
TEST_OUTPUT:
---
fail_compilation/systemvariables.d(30): Error: cannot access `@system` variable `gInt` in @safe code
fail_compilation/systemvariables.d(31): Error: cannot access `@system` variable `gInt` in @safe code
fail_compilation/systemvariables.d(32): Error: cannot access `@system` variable `gArr` in @safe code
fail_compilation/systemvariables.d(33): Error: cannot access `@system` variable `gArr` in @safe code
fail_compilation/systemvariables.d(34): Error: cannot access `@system` variable `gInt` in @safe code
fail_compilation/systemvariables.d(37): Error: cannot access `@system` variable `lSys` in @safe code
fail_compilation/systemvariables.d(38): Error: cannot access `@system` variable `lSys` in @safe code
fail_compilation/systemvariables.d(39): Error: cannot access `@system` variable `lSys` in @safe code
fail_compilation/systemvariables.d(41): Error: cannot access `@system` variable `eInt` in @safe code
---
*/

// http://dlang.org/dips/1035


@system int gInt;
@system enum int eInt = 3;
@system { int[] gArr; }
alias aliasToSys = gInt;

void increment(ref int x) @safe { x++; }
void incrementP(int* x) @safe { (*x)++; }

void basic() @safe
{
    gInt = 0; // error
    gInt++; // error
    gArr ~= 30; // error
    const c = gArr[0]; // error
    aliasToSys++; // error

    @system int lSys = 0;
    lSys = 0; // error
    increment(lSys); // error
    incrementP(&lSys); // error

    int a = eInt; // error
    int b = typeof(eInt).max; // allowed

    void f() @trusted
    {
        lSys = 0; // allowed
    }
}
