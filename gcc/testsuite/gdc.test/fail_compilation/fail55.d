/*
TEST_OUTPUT:
---
fail_compilation/fail55.d(23): Error: function `object.Object.opCmp(Object o)` is not callable using argument types `(int)`
fail_compilation/fail55.d(23):        cannot pass argument `0` of type `int` to parameter `Object o`
---
*/

// $HeadURL$
// $Date$
// $Author$

// @author@ zwang <nehzgnaw@gmail.com>
// @date@   2005-02-03
// @uri@    news:cttjjg$4i0$2@digitaldaemon.com

// __DSTRESS_ELINE__ 14

module dstress.nocompile.bug_mtype_507_D;

void test()
{
    0 < Exception;
}
