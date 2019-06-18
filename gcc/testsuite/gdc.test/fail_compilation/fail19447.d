/* TEST_OUTPUT:
---
fail_compilation/fail19447.d(110): Error: static variable `mh` cannot be read at compile time
fail_compilation/fail19447.d(110):        called from here: `g19447(mh)`
---
 */

#line 100

int [2] mh = [1, 2];
int g19447(ref int[2] a)
{
    int[2] b=2;
    a=b;
    assert(a[0]==2);
    return 1;
}

immutable int i = g19447(mh);
