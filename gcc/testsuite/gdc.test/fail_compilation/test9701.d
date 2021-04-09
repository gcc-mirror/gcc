/*
TEST_OUTPUT
---
fail_compilation/test9701.d(38): Error: `@safe` is not a valid attribute for enum members
fail_compilation/test9701.d(39): Error: `@system` is not a valid attribute for enum members
fail_compilation/test9701.d(40): Error: `@trusted` is not a valid attribute for enum members
fail_compilation/test9701.d(41): Error: `@nogc` is not a valid attribute for enum members
fail_compilation/test9701.d(42): Error: `pure` is not a valid attribute for enum members
fail_compilation/test9701.d(43): Error: `shared` is not a valid attribute for enum members
fail_compilation/test9701.d(44): Error: `inout` is not a valid attribute for enum members
fail_compilation/test9701.d(45): Error: `immutable` is not a valid attribute for enum members
fail_compilation/test9701.d(46): Error: `const` is not a valid attribute for enum members
fail_compilation/test9701.d(47): Error: `synchronized` is not a valid attribute for enum members
fail_compilation/test9701.d(48): Error: `scope` is not a valid attribute for enum members
fail_compilation/test9701.d(49): Error: `auto` is not a valid attribute for enum members
fail_compilation/test9701.d(50): Error: `ref` is not a valid attribute for enum members
fail_compilation/test9701.d(51): Error: `__gshared` is not a valid attribute for enum members
fail_compilation/test9701.d(52): Error: `final` is not a valid attribute for enum members
fail_compilation/test9701.d(53): Error: `extern` is not a valid attribute for enum members
fail_compilation/test9701.d(54): Error: `export` is not a valid attribute for enum members
fail_compilation/test9701.d(55): Error: `nothrow` is not a valid attribute for enum members
fail_compilation/test9701.d(56): Error: `public` is not a valid attribute for enum members
fail_compilation/test9701.d(57): Error: `private` is not a valid attribute for enum members
fail_compilation/test9701.d(58): Error: `package` is not a valid attribute for enum members
fail_compilation/test9701.d(59): Error: `static` is not a valid attribute for enum members
fail_compilation/test9701.d(60): Error: `static` is not a valid attribute for enum members
fail_compilation/test9701.d(61): Error: `static` is not a valid attribute for enum members
fail_compilation/test9701.d(62): Error: `static` is not a valid attribute for enum members
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
