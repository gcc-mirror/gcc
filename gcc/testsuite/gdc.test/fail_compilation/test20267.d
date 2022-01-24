/*
EXTRA_FILES: imports/test20267.d
TEST_OUTPUT:
---
fail_compilation/test20267.d(20): Error: variable `string` is used as a type
fail_compilation/test20267.d(19):        variable `string` is declared here
fail_compilation/test20267.d(23): Error: variable `boolean` is used as a type
fail_compilation/test20267.d(22):        variable `boolean` is declared here
fail_compilation/test20267.d(30): Error: variable `array` is used as a type
fail_compilation/test20267.d(28):        variable `array` is imported here from: `imports.test20267`
fail_compilation/imports/test20267.d(3):        variable `array` is declared here
---
*/

alias boolean = bool;

void foo(string[] args)
{
    immutable string = "bar";
    string[] args2 = args;

    bool boolean = true;
    boolean b = false;
}

void bar()
{
    import imports.test20267 : array;

    array foo;
}
