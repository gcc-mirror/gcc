/* TEST_OUTPUT:
---
fail_compilation/issue22682.d(14): Error: `pragma(mangle)` must be attached to a declaration
fail_compilation/issue22682.d(15): Error: `pragma(mangle)` takes a single argument that must be a string literal
fail_compilation/issue22682.d(16): Error: `string` expected for pragma mangle argument, not `(0)` of type `int`
fail_compilation/issue22682.d(16): Error: `pragma(mangle)` takes a single argument that must be a string literal
fail_compilation/issue22682.d(17): Error: `pragma(mangle)` must be attached to a declaration
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
