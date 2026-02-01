/* TEST_OUTPUT:
---
fail_compilation/issue22682.d(13): Error: `pragma(mangle)` expects string literal argument for mangled name
fail_compilation/issue22682.d(14): Error: `pragma(mangle)` expects string literal argument for mangled name
fail_compilation/issue22682.d(15): Error: `string` expected for pragma mangle argument, not `(0)` of type `int`
fail_compilation/issue22682.d(16): Error: `pragma(mangle)` expects string literal argument for mangled name
---
 */
module issue22682;

void main()
{
    pragma(mangle) {}
    pragma(mangle) static int i0;
    pragma(mangle, 0) static int i1;
    pragma(mangle);
}
