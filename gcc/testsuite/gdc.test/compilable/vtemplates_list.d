/* REQUIRED_ARGS: -vtemplates=list-instances
TEST_OUTPUT:
---
compilable/vtemplates_list.d(19): vtemplate: 4 (3 distinct) instantiation(s) of template `foo(int I)()` found, they are:
compilable/vtemplates_list.d(25): vtemplate: explicit instance `foo!1`
compilable/vtemplates_list.d(26): vtemplate: explicit instance `foo!1`
compilable/vtemplates_list.d(27): vtemplate: explicit instance `foo!2`
compilable/vtemplates_list.d(28): vtemplate: explicit instance `foo!3`
compilable/vtemplates_list.d(20): vtemplate: 3 (1 distinct) instantiation(s) of template `goo1(int I)()` found, they are:
compilable/vtemplates_list.d(30): vtemplate: explicit instance `goo1!1`
compilable/vtemplates_list.d(31): vtemplate: explicit instance `goo1!1`
compilable/vtemplates_list.d(21): vtemplate: implicit instance `goo1!1`
compilable/vtemplates_list.d(21): vtemplate: 2 (1 distinct) instantiation(s) of template `goo2(int I)()` found, they are:
compilable/vtemplates_list.d(33): vtemplate: explicit instance `goo2!1`
compilable/vtemplates_list.d(34): vtemplate: explicit instance `goo2!1`
compilable/vtemplates_list.d(52): vtemplate: 1 (1 distinct) instantiation(s) of template `A()` found, they are:
compilable/vtemplates_list.d-mixin-53(53): vtemplate: explicit instance `A!()`
---
*/

#line 19
void foo(int I)() { }
void goo1(int I)() { }
void goo2(int I)() { goo1!(I); }

void test()
{
    foo!(1)();
    foo!(1)();
    foo!(2)();
    foo!(3)();

    goo1!(1)();
    goo1!(1)();

    goo2!(1)();
    goo2!(1)();
}

// https://issues.dlang.org/show_bug.cgi?id=21489
#line 50
void test2()
{
    template A() {}
    alias ta = mixin("A!()");
}
