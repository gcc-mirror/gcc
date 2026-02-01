// https://github.com/dlang/dmd/issues/21504

// Test that:
// 1. __traits(compiles) tests fail when violating an explicitly set attribute.
// 2. __traits(compiles) tests pass when attribute is inferred (see functions
//   isPureBypassingInference, isSafeBypassingInference, isNogcBypassingInference).
// 3. __traits(compiles) tests do not affect the result of attribute inference,
//    as the they are only evaluated at compile-time, not run-time.

__gshared int testPure;

auto testPure1() pure
{
    return __traits(compiles, testPure = 1);
}
auto testPure2()
{
    return __traits(compiles, testPure = 1);
}

static assert(!testPure1() && testPure2());
static assert(__traits(getFunctionAttributes, testPure1) == __traits(getFunctionAttributes, testPure2));

////////////////////////////

__gshared Exception testNothrow;

auto testNothrow1() nothrow
{
    return __traits(compiles, throw testNothrow);
}
auto testNothrow2()
{
    return __traits(compiles, throw testNothrow);
}

static assert(!testNothrow1() && testNothrow2());
static assert(__traits(getFunctionAttributes, testNothrow1) == __traits(getFunctionAttributes, testNothrow2));

////////////////////////////

__gshared int* testSafe;

auto testSafe1() @safe
{
    return __traits(compiles, testSafe + 1);
}
auto testSafe2()
{
    return __traits(compiles, testSafe + 1);
}

static assert(!testSafe1() && testSafe2());
static assert(__traits(getFunctionAttributes, testSafe1) == __traits(getFunctionAttributes, testSafe2));

////////////////////////////

auto testNogc1() @nogc
{
    return __traits(compiles, [1, 2]);
}
auto testNogc2()
{
    return __traits(compiles, [1, 2]);
}

static assert(!testNogc1() && testNogc2());
static assert(__traits(getFunctionAttributes, testNogc1) == __traits(getFunctionAttributes, testNogc2));
