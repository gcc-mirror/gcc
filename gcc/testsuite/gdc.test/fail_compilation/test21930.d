// https://issues.dlang.org/show_bug.cgi?id=21930
/*
TEST_OUTPUT:
---
fail_compilation/test21930.d(21): Error: variable `string` is used as a type
fail_compilation/test21930.d(15):        variable `string` is declared here
fail_compilation/test21930.d(26): Error: constructor `test21930.R.this(string)` is not callable using argument types `()`
---
*/

alias AliasSeq(T...) = T;

alias TP(alias name) = AliasSeq!name;

int string; // 'string' declared as a variable

alias a = TP!(main);

class R
{
    this(string) { } // so constructor have errors
}

@system main()
{
    new R;
}
