/* TEST_OUTPUT:
---
fail_compilation/fail17969.d(9): Error: no property `sum` for type `fail17969.__lambda6!(int[]).__lambda6.MapResult2!((b) => b)`
---
 * https://issues.dlang.org/show_bug.cgi?id=17969
 */
 

alias fun = a => MapResult2!(b => b).sum;

int[] e;
static assert(!is(typeof(fun(e)) == void));
void foo() { fun(e); }

struct MapResult2(alias fun)
{
    int[] _input;
}
