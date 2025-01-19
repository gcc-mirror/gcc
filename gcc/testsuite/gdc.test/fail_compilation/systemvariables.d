/*
REQUIRED_ARGS: -preview=systemVariables
TEST_OUTPUT:
---
fail_compilation/systemvariables.d(39): Error: access `@system` variable `gInt` is not allowed in a `@safe` function
fail_compilation/systemvariables.d(29):        `gInt` is declared here
fail_compilation/systemvariables.d(40): Error: access `@system` variable `gInt` is not allowed in a `@safe` function
fail_compilation/systemvariables.d(29):        `gInt` is declared here
fail_compilation/systemvariables.d(41): Error: access `@system` variable `gArr` is not allowed in a `@safe` function
fail_compilation/systemvariables.d(31):        `gArr` is declared here
fail_compilation/systemvariables.d(42): Error: access `@system` variable `gArr` is not allowed in a `@safe` function
fail_compilation/systemvariables.d(31):        `gArr` is declared here
fail_compilation/systemvariables.d(43): Error: access `@system` variable `gInt` is not allowed in a `@safe` function
fail_compilation/systemvariables.d(29):        `gInt` is declared here
fail_compilation/systemvariables.d(46): Error: access `@system` variable `lSys` is not allowed in a `@safe` function
fail_compilation/systemvariables.d(45):        `lSys` is declared here
fail_compilation/systemvariables.d(47): Error: access `@system` variable `lSys` is not allowed in a `@safe` function
fail_compilation/systemvariables.d(45):        `lSys` is declared here
fail_compilation/systemvariables.d(48): Error: access `@system` variable `lSys` is not allowed in a `@safe` function
fail_compilation/systemvariables.d(45):        `lSys` is declared here
fail_compilation/systemvariables.d(50): Error: access `@system` variable `eInt` is not allowed in a `@safe` function
fail_compilation/systemvariables.d(30):        `eInt` is declared here
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
