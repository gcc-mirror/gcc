// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/imp21832.d
/*
TEST_OUTPUT:
---
fail_compilation/fail21832.d(4): Deprecation: function `imports.imp21832.fun` is deprecated
fail_compilation/fail21832.d(10): Deprecation: template `imports.imp21832.tpl()(char a)` is deprecated
---
*/
#line 1
int test21832a()
{
    import imports.imp21832 : fun;
    return fun('a');
}

int test21832b()
{
    import imports.imp21832 : tpl;
    return tpl('a');
}
