/* TEST_OUTPUT:
---
fail_compilation/mixinprop.d(12): Error: no property `x` for `mixin Foo!() F;
` of type `void`
---
*/
mixin template Foo() { }

void main()
{
    mixin Foo F;
    F.x;
}
