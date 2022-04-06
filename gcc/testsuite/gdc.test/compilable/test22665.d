// EXTRA_FILES: imports/imp22665.c

// https://issues.dlang.org/show_bug.cgi?id=22665

import imports.imp22665;

E foo1(E e)
{
    return e.A; // with qualification, it is an enum
}

int foo2()
{
    return A; // without qualification, it is an int
}

E foo3(E e)
{
    return E.A; // with qualification, it is an enum
}
