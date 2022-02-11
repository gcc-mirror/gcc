/*
TEST_OUTPUT:
---
fail_compilation/diag_cstyle.d(13): Error: instead of C-style syntax, use D-style `int function(int) fp1`
fail_compilation/diag_cstyle.d(14): Error: instead of C-style syntax, use D-style `int function(int)* fp3`
fail_compilation/diag_cstyle.d(16): Error: instead of C-style syntax, use D-style `int function(int) FP`
fail_compilation/diag_cstyle.d(18): Error: instead of C-style syntax, use D-style `int function() fp`
fail_compilation/diag_cstyle.d(18): Error: instead of C-style syntax, use D-style `int[] arr`
fail_compilation/diag_cstyle.d(20): Error: instead of C-style syntax, use D-style `string[] result`
---
*/

int (*fp1)(int);
int (*(*fp3))(int);

alias int(*FP)(int);

void foo(int(*fp)(), int arr[]) {}

string result[]() = "abc";
