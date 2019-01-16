/*
TEST_OUTPUT:
---
fail_compilation/ice12497.d(15): Error: string expected for argument to mixin, not (foo()) of type void
fail_compilation/ice12497.d(17): Error: string expected for argument to mixin, not (foo()) of type void
---
*/

void foo() {}

void main()
{
    struct S
    {
        mixin(foo());   // MixinDeclaration
    }
    mixin(foo());       // MixinStatement
}
