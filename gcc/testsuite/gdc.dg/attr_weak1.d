// { dg-do compile }

import gcc.attributes;

pragma(inline, true)
@weak int func() // { dg-warning "inline function .func. declared weak" }
{
    return 0;
}

int parm(@weak int a) // { dg-warning ".weak. attribute ignored" }
{
    return a;
}
