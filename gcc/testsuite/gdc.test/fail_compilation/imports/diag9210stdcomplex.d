module imports.diag9210stdcomplex;

import imports.diag9210stdtraits;

struct Complex(T) if (isFloatingPoint!T)
{
    T re;
    T im;
}

// https://issues.dlang.org/show_bug.cgi?id=9210: Complex!real instantiation is incomplete in here,
// because its completion is deferred by an "undefined identifier" error in imports.diag9210b.
Complex!real expi(real y)
{
    return Complex!real(0, 0);
}
