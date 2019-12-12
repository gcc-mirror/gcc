/*
TEST_OUTPUT:
---
fail_compilation/fail54.d(22): Error: incompatible types for ((0) == (Exception)): cannot use '==' with types
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
}
