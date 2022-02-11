/*
TEST_OUTPUT:
---
fail_compilation/diag8684.d(11): Error: found `;` when expecting `)`
fail_compilation/diag8684.d(12): Error: semicolon expected, not `for`
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
