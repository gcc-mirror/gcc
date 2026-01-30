/* REQUIRED_ARGS: -preview=bitfields
TEST_OUTPUT:
---
fail_compilation/fail20365.d(16): Error: bitfield `a.x` cannot be assigned to `ref x`
fail_compilation/fail20365.d(23): Error: function `f` is not callable using argument types `(int)`
fail_compilation/fail20365.d(23):        cannot pass bitfield argument `a.y` to parameter `ref int`
fail_compilation/fail20365.d(19):        `fail20365.f(ref int)` declared here
fail_compilation/fail20365.d(28): Error: cannot `ref` return bitfield `a.y`
---
*/

struct A { int x : 16, y : 16; }

void ref_assign(A a)
{
    ref int x = a.x;
}

void f(ref int);

void ref_param(A a)
{
    f(a.y);
}

ref int ref_return(ref A a)
{
    return a.y;
}
