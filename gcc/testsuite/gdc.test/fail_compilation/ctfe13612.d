/*
TEST_OUTPUT:
---
fail_compilation/ctfe13612.d(15): Error: function `ctfe13612.S.recurse` CTFE recursion limit exceeded
fail_compilation/ctfe13612.d(20):        called from here: `s.recurse()`
fail_compilation/ctfe13612.d(15):        1000 recursive calls to function `recurse`
fail_compilation/ctfe13612.d(23):        called from here: `(new S).recurse()`
fail_compilation/ctfe13612.d(23):        while evaluating: `static assert((new S).recurse())`
---
*/

class S
{
    int x;
    int recurse()
    {
        S s;
        assert(!x); // Error: class 'this' is null and cannot be dereferenced
        s = new S();
        return s.recurse();
    }
}
static assert(new S().recurse());
