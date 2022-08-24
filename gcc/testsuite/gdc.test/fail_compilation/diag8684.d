/*
TEST_OUTPUT:
---
fail_compilation/diag8684.d(12): Error: found `;` when expecting `)`
fail_compilation/diag8684.d(13): Error: semicolon needed to end declaration of `x`, instead of `for`
fail_compilation/diag8684.d(12):        `x` declared here
---
*/

int foo(int n, int m)
{
    int x = foo( 5, m;
    for (int q=0; q<10; ++q){
       ++q;
    }
    return  2;
}
