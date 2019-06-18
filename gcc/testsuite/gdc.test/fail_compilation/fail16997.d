/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/fail16997.d(31): Deprecation: integral promotion not done for `~c`, use '-preview=intpromote' switch or `~cast(int)(c)`
fail_compilation/fail16997.d(32): Deprecation: integral promotion not done for `-c`, use '-preview=intpromote' switch or `-cast(int)(c)`
fail_compilation/fail16997.d(33): Deprecation: integral promotion not done for `+c`, use '-preview=intpromote' switch or `+cast(int)(c)`
fail_compilation/fail16997.d(36): Deprecation: integral promotion not done for `~w`, use '-preview=intpromote' switch or `~cast(int)(w)`
fail_compilation/fail16997.d(37): Deprecation: integral promotion not done for `-w`, use '-preview=intpromote' switch or `-cast(int)(w)`
fail_compilation/fail16997.d(38): Deprecation: integral promotion not done for `+w`, use '-preview=intpromote' switch or `+cast(int)(w)`
fail_compilation/fail16997.d(41): Deprecation: integral promotion not done for `~sb`, use '-preview=intpromote' switch or `~cast(int)(sb)`
fail_compilation/fail16997.d(42): Deprecation: integral promotion not done for `-sb`, use '-preview=intpromote' switch or `-cast(int)(sb)`
fail_compilation/fail16997.d(43): Deprecation: integral promotion not done for `+sb`, use '-preview=intpromote' switch or `+cast(int)(sb)`
fail_compilation/fail16997.d(46): Deprecation: integral promotion not done for `~ub`, use '-preview=intpromote' switch or `~cast(int)(ub)`
fail_compilation/fail16997.d(47): Deprecation: integral promotion not done for `-ub`, use '-preview=intpromote' switch or `-cast(int)(ub)`
fail_compilation/fail16997.d(48): Deprecation: integral promotion not done for `+ub`, use '-preview=intpromote' switch or `+cast(int)(ub)`
fail_compilation/fail16997.d(51): Deprecation: integral promotion not done for `~s`, use '-preview=intpromote' switch or `~cast(int)(s)`
fail_compilation/fail16997.d(52): Deprecation: integral promotion not done for `-s`, use '-preview=intpromote' switch or `-cast(int)(s)`
fail_compilation/fail16997.d(53): Deprecation: integral promotion not done for `+s`, use '-preview=intpromote' switch or `+cast(int)(s)`
fail_compilation/fail16997.d(56): Deprecation: integral promotion not done for `~us`, use '-preview=intpromote' switch or `~cast(int)(us)`
fail_compilation/fail16997.d(57): Deprecation: integral promotion not done for `-us`, use '-preview=intpromote' switch or `-cast(int)(us)`
fail_compilation/fail16997.d(58): Deprecation: integral promotion not done for `+us`, use '-preview=intpromote' switch or `+cast(int)(us)`
---
*/

void test()
{
    int x;

    char c;
    x = ~c;
    x = -c;
    x = +c;

    wchar w;
    x = ~w;
    x = -w;
    x = +w;

    byte sb;
    x = ~sb;
    x = -sb;
    x = +sb;

    ubyte ub;
    x = ~ub;
    x = -ub;
    x = +ub;

    short s;
    x = ~s;
    x = -s;
    x = +s;

    ushort us;
    x = ~us;
    x = -us;
    x = +us;
}
