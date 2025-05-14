/*
TEST_OUTPUT:
---
fail_compilation/fail20.d(17): Error: no operator `<` for type `FOO`
fail_compilation/fail20.d(11):        perhaps overload it with `int opCmp(FOO other) const {}`
---
*/

// ICE(cod3) DMD0.080

struct FOO{}

void main()
{
    FOO one;
    FOO two;
    if (one < two){} // This should tell me that there
                     // is no opCmp() defined instead
                     // of crashing.
}
