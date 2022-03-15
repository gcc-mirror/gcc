// REQUIRED_ARGS: -w -dw
// PERMUTE_ARGS:
/* TEST_OUTPUT:
---
runnable/sctor2.d(12): Deprecation: `scope` as a type constraint is deprecated.  Use `scope` at the usage site.
---
*/

/***************************************************/
// 15665

scope class C15665 (V)
{
    this () {}
}

void test15665()
{
    scope foo = new C15665!int;
}

void main()
{
    test15665();
}
