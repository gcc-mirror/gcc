/*
TEST_OUTPUT:
---
fail_compilation/ice14621.d(22): Error: static assert  `false` is false
fail_compilation/ice14621.d(28):        instantiated from here: erroneousTemplateInstantiation!()
---
*/

void main()
{
    S s;
    s.foo();
}

struct S
{
    float[] array;
    alias array this;

    template erroneousTemplateInstantiation()
    {
        static assert(false);
    }

    void foo()
    {
        S ret;
        ret[] = erroneousTemplateInstantiation!();
    }
}
