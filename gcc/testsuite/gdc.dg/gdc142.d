// https://bugzilla.gdcproject.org/show_bug.cgi?id=142
// { dg-do compile }

import gcc.attributes;

@attribute("noinline")
int test142a()()
{
    return 142;
}

void test142()
{
    enum E142 = test142a();
}
