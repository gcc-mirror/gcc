// COMPILABLE_MATH_TEST
// Test CTFE builtins for std.math functions.

import std.math;

void main()
{
    static assert(approxEqual(sin(2.0L), 0.9092974L));
    static assert(approxEqual(cos(2.0), -0.4161468));
    static assert(approxEqual(tan(2.0f), -2.185040f));
    static assert(approxEqual(sqrt(2.0L), 1.414214L));
    static assert(fabs(-2.0) == 2.0);
    static assert(ldexp(2.5f, 3) == 20.0f);

    static assert(isNaN(real.init));
    static assert(isNaN(double.nan));
    static assert(!isNaN(float.infinity));

    static assert(isInfinity(real.infinity));
    static assert(isInfinity(-double.infinity));
    static assert(!isInfinity(float.nan));

    static assert(isFinite(1.0L));
    static assert(!isFinite(double.infinity));
    static assert(!isFinite(float.nan));
}
