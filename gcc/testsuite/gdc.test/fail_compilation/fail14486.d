// REQUIRED_ARGS: -o-

/*
TEST_OUTPUT:
---
fail_compilation/fail14486.d(47): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(47):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(48): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(48):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(53): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(53):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(54): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(54):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(59): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(59):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(60): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(60):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(65): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(65):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(66): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(66):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(71): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(71):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(72): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(72):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(77): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(77):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/fail14486.d(78): Error: the `delete` keyword is obsolete
fail_compilation/fail14486.d(78):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
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
