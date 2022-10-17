/+
REQUIRED_ARGS: -d
RUN_OUTPUT:
---
float: 1 == 1
double: 1 == 1
real: 1 == 1
ifloat: 1 == 1
idouble: 1 == 1
ireal: 1 == 1
cfloat: 1 == 1
cdouble: 1 == 1
creal: 1 == 1
---
+/

import core.stdc.stdio : printf;

void test(T, string lit)()
{
    T d = mixin(lit);
    bool runtime = cast(bool) d;
    bool folded  = cast(bool) mixin(lit);

    printf((T.stringof ~ ": %d == %d\n\0").ptr, runtime, folded);
}

void main()
{
    test!(float,  "0.5f");
    test!(double, "0.5" );
    test!(real,   "0.5L");

    test!(ifloat,  "0.5i");
    test!(idouble, "0.5i");
    test!(ireal,   "0.5i");

    test!(cfloat,  "0.3 + 0.5i");
    test!(cdouble, "0.3 + 0.5i");
    test!(creal,   "0.3 + 0.5i");
}
