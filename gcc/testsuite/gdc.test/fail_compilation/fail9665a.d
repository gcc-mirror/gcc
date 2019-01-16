// REQUIRED_ARGS:
// PERMUTE_ARGS:

/***************************************************/
// immutable field

/+
TEST_OUTPUT:
---
fail_compilation/fail9665a.d(19): Error: immutable field 'v' initialized multiple times
---
+/
struct S1A
{
    immutable int v;
    this(int)
    {
        v = 1;
        v = 2;  // multiple initialization
    }
}

/+
TEST_OUTPUT:
---
fail_compilation/fail9665a.d(37): Error: immutable field 'v' initialized multiple times
fail_compilation/fail9665a.d(42): Error: immutable field 'v' initialized multiple times
fail_compilation/fail9665a.d(47): Error: immutable field 'v' initialized multiple times
---
+/
struct S1B
{
    immutable int v;
    this(int)
    {
        if (true) v = 1; else v = 2;
        v = 3;  // multiple initialization
    }
    this(long)
    {
        if (true) v = 1;
        v = 3;  // multiple initialization
    }
    this(string)
    {
        if (true) {} else v = 2;
        v = 3;  // multiple initialization
    }
}

/+
TEST_OUTPUT:
---
fail_compilation/fail9665a.d(65): Error: immutable field 'v' initialized multiple times
fail_compilation/fail9665a.d(70): Error: immutable field 'v' initialized multiple times
fail_compilation/fail9665a.d(75): Error: immutable field 'v' initialized multiple times
---
+/
struct S1C
{
    immutable int v;
    this(int)
    {
        true ? (v = 1) : (v = 2);
        v = 3;  // multiple initialization
    }
    this(long)
    {
        auto x = true ? (v = 1) : 2;
        v = 3;  // multiple initialization
    }
    this(string)
    {
        auto x = true ? 1 : (v = 2);
        v = 3;  // multiple initialization
    }
}

/***************************************************/
// with control flow

/+
TEST_OUTPUT:
---
fail_compilation/fail9665a.d(98): Error: immutable field 'v' initialization is not allowed in loops or after labels
fail_compilation/fail9665a.d(103): Error: immutable field 'v' initialization is not allowed in loops or after labels
fail_compilation/fail9665a.d(108): Error: immutable field 'v' initialized multiple times
fail_compilation/fail9665a.d(113): Error: immutable field 'v' initialized multiple times
fail_compilation/fail9665a.d(118): Error: immutable field 'v' initialized multiple times
---
+/
struct S2
{
    immutable int v;
    this(int)
    {
    L:
        v = 1;  // after labels
    }
    this(long)
    {
        foreach (i; 0..1)
            v = 1;  // in loops
    }
    this(string)
    {
        v = 1;  // initialization
    L:  v = 2;  // assignment after labels
    }
    this(wstring)
    {
        v = 1;  // initialization
        foreach (i; 0..1) v = 2;  // assignment in loops
    }
    this(dstring)
    {
        v = 1; return;
        v = 2;  // multiple initialization
    }
}

/***************************************************/
// with immutable constructor

/+
TEST_OUTPUT:
---
fail_compilation/fail9665a.d(139): Error: immutable field 'v' initialized multiple times
fail_compilation/fail9665a.d(143): Error: immutable field 'w' initialized multiple times
---
+/
struct S3
{
    int v;
    int w;
    this(int) immutable
    {
        v = 1;
        v = 2;  // multiple initialization

        if (true)
            w = 1;
        w = 2;  // multiple initialization
    }
}

/***************************************************/
// in __traits(compiles)

/+
TEST_OUTPUT:
---
fail_compilation/fail9665a.d(163): Error: static assert  `__traits(compiles, this.v = 1)` is false
---
+/
struct S4
{
    immutable int v;
    this(int)
    {
        static assert(__traits(compiles, v = 1));
        v = 1;
        static assert(__traits(compiles, v = 1)); // multiple initialization
    }
}

