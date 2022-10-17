// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/imp21832.d
int test21832a()
{
    import imports.imp21832 : fun; // function 'fun' is deprecated
    return fun(0);
}

int test21832b()
{
    import imports.imp21832 : tpl; // template 'tpl' is deprecated
    return tpl(0);
}
