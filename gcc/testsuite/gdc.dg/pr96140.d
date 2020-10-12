// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96140
// { dg-do compile }
module pr94140;

import core.stdc.stdarg;

void test_va_arg(ref int a, ...)
{
    return va_arg!int(_argptr, a);
}

void test_va_start(ref va_list a, ...)
{
    return va_start(a, a);
}
