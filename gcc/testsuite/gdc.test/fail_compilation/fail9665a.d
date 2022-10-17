/+
TEST_OUTPUT:
---
fail_compilation/fail9665a.d(43): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(42):        Previous initialization is here.
fail_compilation/fail9665a.d(53): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(52):        Previous initialization is here.
fail_compilation/fail9665a.d(58): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(57):        Previous initialization is here.
fail_compilation/fail9665a.d(63): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(62):        Previous initialization is here.
fail_compilation/fail9665a.d(73): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(72):        Previous initialization is here.
fail_compilation/fail9665a.d(78): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(77):        Previous initialization is here.
fail_compilation/fail9665a.d(83): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(82):        Previous initialization is here.
fail_compilation/fail9665a.d(96): Error: immutable field `v` initialization is not allowed in loops or after labels
fail_compilation/fail9665a.d(101): Error: immutable field `v` initialization is not allowed in loops or after labels
fail_compilation/fail9665a.d(106): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(105):        Previous initialization is here.
fail_compilation/fail9665a.d(111): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(110):        Previous initialization is here.
fail_compilation/fail9665a.d(116): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(115):        Previous initialization is here.
fail_compilation/fail9665a.d(130): Error: immutable field `v` initialized multiple times
fail_compilation/fail9665a.d(129):        Previous initialization is here.
fail_compilation/fail9665a.d(134): Error: immutable field `w` initialized multiple times
fail_compilation/fail9665a.d(133):        Previous initialization is here.
fail_compilation/fail9665a.d(148): Error: static assert:  `__traits(compiles, this.v = 1)` is false
---
+/

/***************************************************/
// immutable field

struct S1A
{
    immutable int v;
    this(int)
    {
        v = 1;
        v = 2;  // multiple initialization
    }
}

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
