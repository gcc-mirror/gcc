/*
TEST_OUTPUT:
---
fail_compilation/test9701.d(38): Error: `@safe` is not a valid attribute for enum members
fail_compilation/test9701.d(39): Error: `@system` is not a valid attribute for enum members
fail_compilation/test9701.d(40): Error: `@trusted` is not a valid attribute for enum members
fail_compilation/test9701.d(41): Error: `@nogc` is not a valid attribute for enum members
fail_compilation/test9701.d(42): Error: found `pure` when expecting `identifier`
fail_compilation/test9701.d(43): Error: found `shared` when expecting `identifier`
fail_compilation/test9701.d(44): Error: found `inout` when expecting `identifier`
fail_compilation/test9701.d(45): Error: found `immutable` when expecting `identifier`
fail_compilation/test9701.d(46): Error: found `const` when expecting `identifier`
fail_compilation/test9701.d(47): Error: found `synchronized` when expecting `identifier`
fail_compilation/test9701.d(48): Error: found `scope` when expecting `identifier`
fail_compilation/test9701.d(49): Error: found `auto` when expecting `identifier`
fail_compilation/test9701.d(50): Error: found `ref` when expecting `identifier`
fail_compilation/test9701.d(51): Error: found `__gshared` when expecting `identifier`
fail_compilation/test9701.d(52): Error: found `final` when expecting `identifier`
fail_compilation/test9701.d(53): Error: found `extern` when expecting `identifier`
fail_compilation/test9701.d(54): Error: found `export` when expecting `identifier`
fail_compilation/test9701.d(55): Error: found `nothrow` when expecting `identifier`
fail_compilation/test9701.d(56): Error: found `public` when expecting `identifier`
fail_compilation/test9701.d(57): Error: found `private` when expecting `identifier`
fail_compilation/test9701.d(58): Error: found `package` when expecting `identifier`
fail_compilation/test9701.d(59): Error: found `static` when expecting `identifier`
fail_compilation/test9701.d(60): Error: found `static` when expecting `identifier`
fail_compilation/test9701.d(61): Error: found `static` when expecting `identifier`
fail_compilation/test9701.d(62): Error: found `static` when expecting `identifier`
---
*/

// This test exists to verify that parsing of enum member attributes rejects invalid attributes

// https://issues.dlang.org/show_bug.cgi?id=9701

enum Enum
{
    @safe safe,
    @system system,
    @trusted trusted,
    @nogc nogc,
    pure pure_,
    shared shared_,
    inout inout_,
    immutable immutable_,
    const const_,
    synchronized synchronized_,
    scope scope_,
    auto auto_,
    ref ref_,
    __gshared __gshared_,
    final final_,
    extern extern_,
    export export_,
    nothrow nothrow_,
    public public_,
    private private_,
    package package_,
    static static1,
    @("a") static static2,
    static @("a") static3,
    @("a") static @("b") static3,
}
