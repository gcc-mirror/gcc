/* REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22680.d(104): Error: scope variable `this` assigned to global variable `c`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=22680

#line 100

C c;
class C {
    ~this() @safe {
	    c = this;
    }
}
