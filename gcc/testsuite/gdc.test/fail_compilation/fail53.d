/*
TEST_OUTPUT:
---
fail_compilation/fail53.d(25): Error: function object.Object.opEquals (Object o) is not callable using argument types (int)
---
*/

// $HeadURL$
// $Date$
// $Author$

// @author@	Thomas Kuehne <thomas-dloop@kuehne.thisisspam.cn>
// @date@	2005-01-22
// @uri@	news:csvvet$2g4$1@digitaldaemon.com
// @url@	nntp://news.digitalmars.com/digitalmars.D.bugs/2741

// __DSTRESS_ELINE__ 17

module dstress.nocompile.bug_mtype_507_A;

int main()
{
    Object o;
    int i;
    if (i == o)
    {
        return -1;
    }
    return 0;
}
