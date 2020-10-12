// https://bugzilla.gdcproject.org/show_bug.cgi?id=47
// { dg-do compile }

template Foo47()
{
    void test47()
    {
        asm { "nop"; }
    }
}

mixin Foo47!();
