// REQUIRED_ARGS: -o-

/*
TEST_OUTPUT:
---
fail_compilation/fail14486.d(56): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(56): Error: `delete c0` is not `@safe` but is used in `@safe` function `test1a`
fail_compilation/fail14486.d(57): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(57): Error: `pure` function `fail14486.test1a` cannot call impure destructor `fail14486.C1a.~this`
fail_compilation/fail14486.d(57): Error: `@safe` function `fail14486.test1a` cannot call `@system` destructor `fail14486.C1a.~this`
fail_compilation/fail14486.d(43):        `fail14486.C1a.~this` is declared here
fail_compilation/fail14486.d(57): Error: `@nogc` function `fail14486.test1a` cannot call non-@nogc destructor `fail14486.C1a.~this`
fail_compilation/fail14486.d(62): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(63): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(63): Error: destructor `fail14486.C1b.~this` is not `nothrow`
fail_compilation/fail14486.d(60): Error: `nothrow` function `fail14486.test1b` may throw
fail_compilation/fail14486.d(68): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(68): Error: `delete s0` is not `@safe` but is used in `@safe` function `test2a`
fail_compilation/fail14486.d(69): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(69): Error: `pure` function `fail14486.test2a` cannot call impure destructor `fail14486.S1a.~this`
fail_compilation/fail14486.d(69): Error: `@safe` function `fail14486.test2a` cannot call `@system` destructor `fail14486.S1a.~this`
fail_compilation/fail14486.d(49):        `fail14486.S1a.~this` is declared here
fail_compilation/fail14486.d(69): Error: `@nogc` function `fail14486.test2a` cannot call non-@nogc destructor `fail14486.S1a.~this`
fail_compilation/fail14486.d(74): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(75): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(75): Error: destructor `fail14486.S1b.~this` is not `nothrow`
fail_compilation/fail14486.d(72): Error: `nothrow` function `fail14486.test2b` may throw
fail_compilation/fail14486.d(80): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(80): Error: `delete a0` is not `@safe` but is used in `@safe` function `test3a`
fail_compilation/fail14486.d(81): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(81): Error: `pure` function `fail14486.test3a` cannot call impure destructor `fail14486.S1a.~this`
fail_compilation/fail14486.d(81): Error: `@safe` function `fail14486.test3a` cannot call `@system` destructor `fail14486.S1a.~this`
fail_compilation/fail14486.d(49):        `fail14486.S1a.~this` is declared here
fail_compilation/fail14486.d(81): Error: `@nogc` function `fail14486.test3a` cannot call non-@nogc destructor `fail14486.S1a.~this`
fail_compilation/fail14486.d(86): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(87): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail14486.d(87): Error: destructor `fail14486.S1b.~this` is not `nothrow`
fail_compilation/fail14486.d(84): Error: `nothrow` function `fail14486.test3b` may throw
---
*/

class  C0a { }
class  C1a {                  ~this() {} }

class  C0b { }
class  C1b {                  ~this() {} }

struct S0a { }
struct S1a {                  ~this() {} }

struct S0b { }
struct S1b {                  ~this() {} }

void test1a() @nogc pure @safe
{
    C0a   c0;  delete c0;   // error
    C1a   c1;  delete c1;   // error
}

void test1b() nothrow
{
    C0b   c0;  delete c0;    // no error
    C1b   c1;  delete c1;    // error
}

void test2a() @nogc pure @safe
{
    S0a*  s0;  delete s0;   // error
    S1a*  s1;  delete s1;   // error
}

void test2b() nothrow
{
    S0b*  s0;  delete s0;    // no error
    S1b*  s1;  delete s1;    // error
}

void test3a() @nogc pure @safe
{
    S0a[] a0;  delete a0;   // error
    S1a[] a1;  delete a1;   // error
}

void test3b() nothrow
{
    S0b[] a0;  delete a0;    // no error
    S1b[] a1;  delete a1;    // error
}
