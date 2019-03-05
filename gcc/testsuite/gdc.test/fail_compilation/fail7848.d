// REQUIRED_ARGS: -unittest

/*
TEST_OUTPUT:
---
fail_compilation/fail7848.d(35): Error: pure function 'fail7848.C.__unittestL33_$n$' cannot call impure function 'fail7848.func'
fail_compilation/fail7848.d(35): Error: @safe function 'fail7848.C.__unittestL33_$n$' cannot call @system function 'fail7848.func'
fail_compilation/fail7848.d(35): Error: @nogc function 'fail7848.C.__unittestL33_$n$' cannot call non-@nogc function 'fail7848.func'
fail_compilation/fail7848.d(35): Error: function `fail7848.func` is not nothrow
fail_compilation/fail7848.d(33): Error: nothrow function `fail7848.C.__unittestL33_$n$` may throw
fail_compilation/fail7848.d(40): Error: pure function 'fail7848.C.__invariant2' cannot call impure function 'fail7848.func'
fail_compilation/fail7848.d(40): Error: @safe function 'fail7848.C.__invariant2' cannot call @system function 'fail7848.func'
fail_compilation/fail7848.d(40): Error: @nogc function 'fail7848.C.__invariant2' cannot call non-@nogc function 'fail7848.func'
fail_compilation/fail7848.d(40): Error: function `fail7848.func` is not nothrow
fail_compilation/fail7848.d(38): Error: nothrow function `fail7848.C.__invariant2` may throw
fail_compilation/fail7848.d(45): Error: pure allocator 'fail7848.C.new' cannot call impure function 'fail7848.func'
fail_compilation/fail7848.d(45): Error: @safe allocator 'fail7848.C.new' cannot call @system function 'fail7848.func'
fail_compilation/fail7848.d(45): Error: @nogc allocator 'fail7848.C.new' cannot call non-@nogc function 'fail7848.func'
fail_compilation/fail7848.d(45): Error: function `fail7848.func` is not nothrow
fail_compilation/fail7848.d(43): Error: nothrow allocator `fail7848.C.new` may throw
fail_compilation/fail7848.d(51): Error: pure deallocator 'fail7848.C.delete' cannot call impure function 'fail7848.func'
fail_compilation/fail7848.d(51): Error: @safe deallocator 'fail7848.C.delete' cannot call @system function 'fail7848.func'
fail_compilation/fail7848.d(51): Error: @nogc deallocator 'fail7848.C.delete' cannot call non-@nogc function 'fail7848.func'
fail_compilation/fail7848.d(51): Error: function `fail7848.func` is not nothrow
fail_compilation/fail7848.d(49): Error: nothrow deallocator `fail7848.C.delete` may throw
---
*/

void func() {}

class C
{
    @safe pure nothrow @nogc unittest
    {
        func();
    }

    @safe pure nothrow @nogc invariant
    {
        func();
    }

    @safe pure nothrow @nogc new (size_t sz)
    {
        func();
        return null;
    }

    @safe pure nothrow @nogc delete (void* p)
    {
        func();
    }
}
