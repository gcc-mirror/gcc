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
class  C2a {                  ~this() {}  @nogc pure @safe delete(void* p) {} }
class  C3a { @nogc pure @safe ~this() {}                   delete(void* p) {} }
class  C4a { @nogc pure @safe ~this() {}  @nogc pure @safe delete(void* p) {} }

class  C0b { }
class  C1b {                  ~this() {} }
class  C2b {                  ~this() {}           nothrow delete(void* p) {} }
class  C3b {          nothrow ~this() {}                   delete(void* p) {} }
class  C4b {          nothrow ~this() {}           nothrow delete(void* p) {} }

struct S0a { }
struct S1a {                  ~this() {} }
struct S2a {                  ~this() {}  @nogc pure @safe delete(void* p) {} }
struct S3a { @nogc pure @safe ~this() {}                   delete(void* p) {} }
struct S4a { @nogc pure @safe ~this() {}  @nogc pure @safe delete(void* p) {} }

struct S0b { }
struct S1b {                  ~this() {} }
struct S2b {                  ~this() {}           nothrow delete(void* p) {} }
struct S3b {          nothrow ~this() {}                   delete(void* p) {} }
struct S4b {          nothrow ~this() {}           nothrow delete(void* p) {} }

/*
TEST_OUTPUT:
---
fail_compilation/fail14486.d(44): Error: delete c0 is not @safe but is used in @safe function test1a
fail_compilation/fail14486.d(45): Error: pure function 'fail14486.test1a' cannot call impure destructor 'fail14486.C1a.~this'
fail_compilation/fail14486.d(45): Error: @safe function 'fail14486.test1a' cannot call @system destructor 'fail14486.C1a.~this'
fail_compilation/fail14486.d(45): Error: @nogc function 'fail14486.test1a' cannot call non-@nogc destructor 'fail14486.C1a.~this'
fail_compilation/fail14486.d(46): Error: pure function 'fail14486.test1a' cannot call impure destructor 'fail14486.C2a.~this'
fail_compilation/fail14486.d(46): Error: @safe function 'fail14486.test1a' cannot call @system destructor 'fail14486.C2a.~this'
fail_compilation/fail14486.d(46): Error: @nogc function 'fail14486.test1a' cannot call non-@nogc destructor 'fail14486.C2a.~this'
fail_compilation/fail14486.d(47): Error: pure function 'fail14486.test1a' cannot call impure deallocator 'fail14486.C3a.delete'
fail_compilation/fail14486.d(47): Error: @safe function 'fail14486.test1a' cannot call @system deallocator 'fail14486.C3a.delete'
fail_compilation/fail14486.d(47): Error: @nogc function 'fail14486.test1a' cannot call non-@nogc deallocator 'fail14486.C3a.delete'
fail_compilation/fail14486.d(48): Error: delete c4 is not @safe but is used in @safe function test1a
---*/
void test1a() @nogc pure @safe
{
    C0a   c0;  delete c0;   // error
    C1a   c1;  delete c1;   // error
    C2a   c2;  delete c2;   // error
    C3a   c3;  delete c3;   // error
    C4a   c4;  delete c4;   // no error
}

/*
TEST_OUTPUT:
---
fail_compilation/fail14486.d(63): Error: destructor `fail14486.C1b.~this` is not nothrow
fail_compilation/fail14486.d(64): Error: destructor `fail14486.C2b.~this` is not nothrow
fail_compilation/fail14486.d(65): Error: deallocator `fail14486.C3b.delete` is not nothrow
fail_compilation/fail14486.d(60): Error: nothrow function `fail14486.test1b` may throw
---
*/
void test1b() nothrow
{
    C0b   c0;  delete c0;    // no error
    C1b   c1;  delete c1;    // error
    C2b   c2;  delete c2;    // error
    C3b   c3;  delete c3;    // error
    C4b   c4;  delete c4;    // no error
}

/*
TEST_OUTPUT:
---
fail_compilation/fail14486.d(86): Error: delete s0 is not @safe but is used in @safe function test2a
fail_compilation/fail14486.d(87): Error: pure function 'fail14486.test2a' cannot call impure destructor 'fail14486.S1a.~this'
fail_compilation/fail14486.d(87): Error: @safe function 'fail14486.test2a' cannot call @system destructor 'fail14486.S1a.~this'
fail_compilation/fail14486.d(87): Error: @nogc function 'fail14486.test2a' cannot call non-@nogc destructor 'fail14486.S1a.~this'
fail_compilation/fail14486.d(88): Error: pure function 'fail14486.test2a' cannot call impure destructor 'fail14486.S2a.~this'
fail_compilation/fail14486.d(88): Error: @safe function 'fail14486.test2a' cannot call @system destructor 'fail14486.S2a.~this'
fail_compilation/fail14486.d(88): Error: @nogc function 'fail14486.test2a' cannot call non-@nogc destructor 'fail14486.S2a.~this'
fail_compilation/fail14486.d(89): Error: pure function 'fail14486.test2a' cannot call impure deallocator 'fail14486.S3a.delete'
fail_compilation/fail14486.d(89): Error: @safe function 'fail14486.test2a' cannot call @system deallocator 'fail14486.S3a.delete'
fail_compilation/fail14486.d(89): Error: @nogc function 'fail14486.test2a' cannot call non-@nogc deallocator 'fail14486.S3a.delete'
---
*/
void test2a() @nogc pure @safe
{
    S0a*  s0;  delete s0;   // error
    S1a*  s1;  delete s1;   // error
    S2a*  s2;  delete s2;   // error
    S3a*  s3;  delete s3;   // error
    S4a*  s4;  delete s4;   // no error
}

/*
TEST_OUTPUT:
---
fail_compilation/fail14486.d(105): Error: destructor `fail14486.S1b.~this` is not nothrow
fail_compilation/fail14486.d(106): Error: destructor `fail14486.S2b.~this` is not nothrow
fail_compilation/fail14486.d(107): Error: deallocator `fail14486.S3b.delete` is not nothrow
fail_compilation/fail14486.d(102): Error: nothrow function `fail14486.test2b` may throw
---
*/
void test2b() nothrow
{
    S0b*  s0;  delete s0;    // no error
    S1b*  s1;  delete s1;    // error
    S2b*  s2;  delete s2;    // error
    S3b*  s3;  delete s3;    // error
    S4b*  s4;  delete s4;    // no error
}

/*
TEST_OUTPUT:
---
fail_compilation/fail14486.d(127): Error: delete a0 is not @safe but is used in @safe function test3a
fail_compilation/fail14486.d(128): Error: pure function 'fail14486.test3a' cannot call impure destructor 'fail14486.S1a.~this'
fail_compilation/fail14486.d(128): Error: @safe function 'fail14486.test3a' cannot call @system destructor 'fail14486.S1a.~this'
fail_compilation/fail14486.d(128): Error: @nogc function 'fail14486.test3a' cannot call non-@nogc destructor 'fail14486.S1a.~this'
fail_compilation/fail14486.d(129): Error: pure function 'fail14486.test3a' cannot call impure destructor 'fail14486.S2a.~this'
fail_compilation/fail14486.d(129): Error: @safe function 'fail14486.test3a' cannot call @system destructor 'fail14486.S2a.~this'
fail_compilation/fail14486.d(129): Error: @nogc function 'fail14486.test3a' cannot call non-@nogc destructor 'fail14486.S2a.~this'
fail_compilation/fail14486.d(130): Error: delete a3 is not @safe but is used in @safe function test3a
fail_compilation/fail14486.d(131): Error: delete a4 is not @safe but is used in @safe function test3a
---
*/
void test3a() @nogc pure @safe
{
    S0a[] a0;  delete a0;   // error
    S1a[] a1;  delete a1;   // error
    S2a[] a2;  delete a2;   // error
    S3a[] a3;  delete a3;   // error
    S4a[] a4;  delete a4;   // error
}

/*
TEST_OUTPUT:
---
fail_compilation/fail14486.d(145): Error: destructor `fail14486.S1b.~this` is not nothrow
fail_compilation/fail14486.d(146): Error: destructor `fail14486.S2b.~this` is not nothrow
fail_compilation/fail14486.d(142): Error: nothrow function `fail14486.test3b` may throw
---
*/
void test3b() nothrow
{
    S0b[] a0;  delete a0;    // no error
    S1b[] a1;  delete a1;    // error
    S2b[] a2;  delete a2;    // error
    S3b[] a3;  delete a3;    // no error
    S4b[] a4;  delete a4;    // no error
}
