// REQUIRED_ARGS: -o-

/***************** NewExp *******************/

struct S1 { }
struct S2 { this(int); }
struct S3 { this(int) @nogc; }

/*
TEST_OUTPUT:
---
fail_compilation/nogc1.d(23): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNew`
fail_compilation/nogc1.d(25): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNew`
fail_compilation/nogc1.d(26): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNew`
fail_compilation/nogc1.d(28): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNew`
fail_compilation/nogc1.d(29): Error: `@nogc` function `nogc1.testNew` cannot call non-@nogc constructor `nogc1.S2.this`
fail_compilation/nogc1.d(30): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNew`
fail_compilation/nogc1.d(32): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNew`
---
*/
@nogc void testNew()
{
    int* p1 = new int;

    int[] a1 = new int[3];
    int[][] a2 = new int[][](2, 3);

    S1* ps1 = new S1();
    S2* ps2 = new S2(1);
    S3* ps3 = new S3(1);

    Object o1 = new Object();
}

/*
TEST_OUTPUT:
---
fail_compilation/nogc1.d(48): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNewScope`
fail_compilation/nogc1.d(50): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNewScope`
fail_compilation/nogc1.d(51): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNewScope`
fail_compilation/nogc1.d(53): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNewScope`
fail_compilation/nogc1.d(54): Error: `@nogc` function `nogc1.testNewScope` cannot call non-@nogc constructor `nogc1.S2.this`
fail_compilation/nogc1.d(55): Error: allocating with `new` causes a GC allocation in `@nogc` function `testNewScope`
---
*/
@nogc void testNewScope()
{
    scope int* p1 = new int;

    scope int[] a1 = new int[3];
    scope int[][] a2 = new int[][](2, 3);

    scope S1* ps1 = new S1();
    scope S2* ps2 = new S2(1);
    scope S3* ps3 = new S3(1);

    scope Object o1 = new Object();     // no error
    scope o2 = new Object();            // no error
}

/***************** DeleteExp *******************/

/*
TEST_OUTPUT:
---
fail_compilation/nogc1.d(76): Error: the `delete` keyword is obsolete
fail_compilation/nogc1.d(76):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/nogc1.d(77): Error: the `delete` keyword is obsolete
fail_compilation/nogc1.d(77):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
fail_compilation/nogc1.d(78): Error: the `delete` keyword is obsolete
fail_compilation/nogc1.d(78):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
---
*/
@nogc void testDelete(int* p, Object o, S1* s)
{
    delete p;
    delete o;
    delete s;
}
