/*
TEST_OUTPUT:
---
fail_compilation/fail54.d(27): Error: incompatible types for `(0) == (Exception)`: cannot use `==` with types
fail_compilation/fail54.d(28): Error: incompatible types for `("") == (int)`: cannot use `==` with types
fail_compilation/fail54.d(29): Error: incompatible types for `(true) == (int)`: cannot use `==` with types
fail_compilation/fail54.d(29):        while evaluating: `static assert(true == (int))`
fail_compilation/fail54.d(30): Error: incompatible types for `(true) == (int[string])`: cannot use `==` with types
fail_compilation/fail54.d(30):        while evaluating: `static assert(true == (int[string]))`
---
*/

// $HeadURL$
// $Date$
// $Author$

// @author@	zwang <nehzgnaw@gmail.com>
// @date@	2005-02-03
// @uri@	news:ctthp6$25b$1@digitaldaemon.com

// __DSTRESS_ELINE__ 14

module dstress.nocompile.bug_mtype_507_C;

void test()
{
    0 == Exception;
    "" == int;
    static assert(is(int) == int);
    static assert(is(int[string]) == int[string]);
}
