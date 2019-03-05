// REQUIRED_SRGS: -o-


// references
alias P = int*;             P p;
alias FP = int function();  FP fp;
alias DG = int delegate();  DG dg;
alias DA = int[];           DA da;
alias AA = int[int];        AA aa;
class C {}                  C c;
alias N = typeof(null);     N n;

// values
alias SA = int[1];          SA sa;
struct S {}                 S s;
                            int i;
                            double f;


/*
TEST_OUTPUT:
---
fail_compilation/fail_casting1.d(39): Error: cannot cast expression `p` of type `int*` to `int[1]`
fail_compilation/fail_casting1.d(40): Error: cannot cast expression `fp` of type `int function()` to `int[1]`
fail_compilation/fail_casting1.d(41): Error: cannot cast expression `dg` of type `int delegate()` to `int[1]`
fail_compilation/fail_casting1.d(42): Error: cannot cast expression `da` of type `int[]` to `int[1]`
fail_compilation/fail_casting1.d(43): Error: cannot cast expression `aa` of type `int[int]` to `int[1]`
fail_compilation/fail_casting1.d(44): Error: cannot cast expression `c` of type `fail_casting1.C` to `int[1]`
fail_compilation/fail_casting1.d(45): Error: cannot cast expression `n` of type `typeof(null)` to `int[1]`
fail_compilation/fail_casting1.d(49): Error: cannot cast expression `sa` of type `int[1]` to `int delegate()`
fail_compilation/fail_casting1.d(51): Error: cannot cast expression `sa` of type `int[1]` to `double[]` since sizes don't line up
fail_compilation/fail_casting1.d(52): Error: cannot cast expression `sa` of type `int[1]` to `int[int]`
fail_compilation/fail_casting1.d(53): Error: cannot cast expression `sa` of type `int[1]` to `fail_casting1.C`
fail_compilation/fail_casting1.d(54): Error: cannot cast expression `sa` of type `int[1]` to `typeof(null)`
---
*/
void test1()
{
    { auto x = cast(SA) p; }        // Reject (Bugzilla 14596)
    { auto x = cast(SA)fp; }        // Reject (Bugzilla 14596) (FP is Tpointer)
    { auto x = cast(SA)dg; }        // Reject (from e2ir)
    { auto x = cast(SA)da; }        // Reject (from e2ir)
    { auto x = cast(SA)aa; }        // Reject (from e2ir)
    { auto x = cast(SA) c; }        // Reject (Bugzilla 10646)
    { auto x = cast(SA) n; }        // Reject (Bugzilla 8179)
    { auto x = cast( P)sa; }        // Accept (equivalent with: cast(int*)sa.ptr;)
    { auto x = cast(double*)sa; }   // Accept (equivalent with: cast(double*)sa.ptr;)
    { auto x = cast(FP)sa; }        // Accept (equivalent with: cast(FP)sa.ptr;)
    { auto x = cast(DG)sa; }        // Reject (from e2ir)
    { auto x = cast(DA)sa; }        // Accept (equivalent with: cast(int[])sa[];)
    { auto x = cast(double[])sa; }  // Reject (from e2ir)
    { auto x = cast(AA)sa; }        // Reject (from e2ir)
    { auto x = cast( C)sa; }        // Reject (Bugzilla 10646)
    { auto x = cast( N)sa; }        // Reject (Bugzilla 8179)
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_casting1.d(78): Error: cannot cast expression `p` of type `int*` to `S`
fail_compilation/fail_casting1.d(79): Error: cannot cast expression `fp` of type `int function()` to `S`
fail_compilation/fail_casting1.d(80): Error: cannot cast expression `dg` of type `int delegate()` to `S`
fail_compilation/fail_casting1.d(81): Error: cannot cast expression `da` of type `int[]` to `S`
fail_compilation/fail_casting1.d(82): Error: cannot cast expression `aa` of type `int[int]` to `S`
fail_compilation/fail_casting1.d(83): Error: cannot cast expression `c` of type `fail_casting1.C` to `S`
fail_compilation/fail_casting1.d(84): Error: cannot cast expression `n` of type `typeof(null)` to `S`
fail_compilation/fail_casting1.d(85): Error: cannot cast expression `s` of type `S` to `int*`
fail_compilation/fail_casting1.d(86): Error: cannot cast expression `s` of type `S` to `int function()`
fail_compilation/fail_casting1.d(87): Error: cannot cast expression `s` of type `S` to `int delegate()`
fail_compilation/fail_casting1.d(88): Error: cannot cast expression `s` of type `S` to `int[]`
fail_compilation/fail_casting1.d(89): Error: cannot cast expression `s` of type `S` to `int[int]`
fail_compilation/fail_casting1.d(90): Error: cannot cast expression `s` of type `S` to `fail_casting1.C`
fail_compilation/fail_casting1.d(91): Error: cannot cast expression `s` of type `S` to `typeof(null)`
---
*/
void test2()
{
    { auto x = cast( S) p; }        // Reject (Bugzilla 13959)
    { auto x = cast( S)fp; }        // Reject (Bugzilla 13959) (FP is Tpointer)
    { auto x = cast( S)dg; }        // Reject (from e2ir)
    { auto x = cast( S)da; }        // Reject (from e2ir)
    { auto x = cast( S)aa; }        // Reject (from e2ir)
    { auto x = cast( S) c; }        // Reject (from e2ir)
    { auto x = cast( S) n; }        // Reject (Bugzilla 9904)
    { auto x = cast( P) s; }        // Reject (Bugzilla 13959)
    { auto x = cast(FP) s; }        // Reject (Bugzilla 13959) (FP is Tpointer)
    { auto x = cast(DG) s; }        // Reject (from e2ir)
    { auto x = cast(DA) s; }        // Reject (from e2ir)
    { auto x = cast(AA) s; }        // Reject (from e2ir)
    { auto x = cast( C) s; }        // Reject (from e2ir)
    { auto x = cast( N) s; }        // Reject (Bugzilla 9904)
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_casting1.d(125): Error: cannot cast expression `p` of type `int*` to `int delegate()`
fail_compilation/fail_casting1.d(126): Error: cannot cast expression `p` of type `int*` to `int[]`
fail_compilation/fail_casting1.d(129): Error: cannot cast expression `p` of type `int*` to `typeof(null)`
fail_compilation/fail_casting1.d(133): Error: cannot cast expression `fp` of type `int function()` to `int delegate()`
fail_compilation/fail_casting1.d(134): Error: cannot cast expression `fp` of type `int function()` to `int[]`
fail_compilation/fail_casting1.d(137): Error: cannot cast expression `fp` of type `int function()` to `typeof(null)`
fail_compilation/fail_casting1.d(139): Deprecation: casting from int delegate() to int* is deprecated
fail_compilation/fail_casting1.d(140): Deprecation: casting from int delegate() to int function() is deprecated
fail_compilation/fail_casting1.d(142): Error: cannot cast expression `dg` of type `int delegate()` to `int[]`
fail_compilation/fail_casting1.d(143): Error: cannot cast expression `dg` of type `int delegate()` to `int[int]`
fail_compilation/fail_casting1.d(144): Error: cannot cast expression `dg` of type `int delegate()` to `fail_casting1.C`
fail_compilation/fail_casting1.d(145): Error: cannot cast expression `dg` of type `int delegate()` to `typeof(null)`
fail_compilation/fail_casting1.d(157): Error: cannot cast expression `da` of type `int[]` to `int delegate()`
fail_compilation/fail_casting1.d(159): Error: cannot cast expression `da` of type `int[]` to `int[int]`
fail_compilation/fail_casting1.d(160): Error: cannot cast expression `da` of type `int[]` to `fail_casting1.C`
fail_compilation/fail_casting1.d(161): Error: cannot cast expression `da` of type `int[]` to `typeof(null)`
fail_compilation/fail_casting1.d(165): Error: cannot cast expression `aa` of type `int[int]` to `int delegate()`
fail_compilation/fail_casting1.d(166): Error: cannot cast expression `aa` of type `int[int]` to `int[]`
fail_compilation/fail_casting1.d(169): Error: cannot cast expression `aa` of type `int[int]` to `typeof(null)`
fail_compilation/fail_casting1.d(173): Error: cannot cast expression `c` of type `fail_casting1.C` to `int delegate()`
fail_compilation/fail_casting1.d(174): Error: cannot cast expression `c` of type `fail_casting1.C` to `int[]`
fail_compilation/fail_casting1.d(177): Error: cannot cast expression `c` of type `fail_casting1.C` to `typeof(null)`
---
*/
void test3()    // between reference types
{
    { auto x = cast( P) p; }    // Accept
    { auto x = cast(FP) p; }    // Accept (FP is Tpointer)
    { auto x = cast(DG) p; }    // Reject (from e2ir)
    { auto x = cast(DA) p; }    // Reject (Bugzilla 14596)
    { auto x = cast(AA) p; }    // Accept (because of size match)
    { auto x = cast( C) p; }    // Accept (because of size match)
    { auto x = cast( N) p; }    // Reject (Bugzilla 14629)

    { auto x = cast( P)fp; }    // Accept (FP is Tpointer)
    { auto x = cast(FP)fp; }    // Accept
    { auto x = cast(DG)fp; }    // Reject (from e2ir)
    { auto x = cast(DA)fp; }    // Reject (Bugzilla 14596)
    { auto x = cast(AA)fp; }    // Accept (because of size match)
    { auto x = cast( C)fp; }    // Accept (because of size match)
    { auto x = cast( N)fp; }    // Reject (Bugzilla 14629)

    { auto x = cast( P)dg; }    // Deprecated (equivalent with: cast( P)dg.ptr;)
    { auto x = cast(FP)dg; }    // Deprecated (equivalent with: cast(FP)dg.ptr;)
    { auto x = cast(DG)dg; }    // Accept
    { auto x = cast(DA)dg; }    // Reject (from e2ir)
    { auto x = cast(AA)dg; }    // Reject (from e2ir)
    { auto x = cast( C)dg; }    // Reject (from e2ir)
    { auto x = cast( N)dg; }    // Reject (Bugzilla 14629)

    { auto x = cast( P) n; }    // Accept
    { auto x = cast(FP) n; }    // Accept
    { auto x = cast(DG) n; }    // Accept
    { auto x = cast(DA) n; }    // Accept
    { auto x = cast(AA) n; }    // Accept
    { auto x = cast( C) n; }    // Accept
    { auto x = cast( N) n; }    // Accept

    { auto x = cast( P)da; }    // Accept (equivalent with: cast(P)da.ptr;)
    { auto x = cast(FP)da; }    // Accept (FP is Tpointer)
    { auto x = cast(DG)da; }    // Reject (from e2ir)
    { auto x = cast(DA)da; }    // Accept
    { auto x = cast(AA)da; }    // Reject (from e2ir)
    { auto x = cast( C)da; }    // Reject (Bugzilla 10646)
    { auto x = cast( N)da; }    // Reject (Bugzilla 14629)

    { auto x = cast( P)aa; }    // Accept (because of size match)
    { auto x = cast(FP)aa; }    // Accept (FP is Tpointer)
    { auto x = cast(DG)aa; }    // Reject (from e2ir)
    { auto x = cast(DA)aa; }    // Reject (from e2ir)
    { auto x = cast(AA)aa; }    // Accept
    { auto x = cast( C)aa; }    // Accept (because of size match)
    { auto x = cast( N)aa; }    // Reject (Bugzilla 14629)

    { auto x = cast( P) c; }    // Accept
    { auto x = cast(FP) c; }    // Accept (FP is Tpointer)
    { auto x = cast(DG) c; }    // Reject (from e2ir)
    { auto x = cast(DA) c; }    // Reject (Bugzilla 10646)
    { auto x = cast(AA) c; }    // Accept (because of size match)
    { auto x = cast( C) c; }    // Accept
    { auto x = cast( N) c; }    // Reject (Bugzilla 14629)
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_casting1.d(206): Error: cannot cast expression `0` of type `int` to `int delegate()`
fail_compilation/fail_casting1.d(207): Error: cannot cast expression `0` of type `int` to `int[]`
fail_compilation/fail_casting1.d(208): Error: cannot cast expression `0` of type `int` to `int[1]`
fail_compilation/fail_casting1.d(209): Error: cannot cast expression `0` of type `int` to `int[int]`
fail_compilation/fail_casting1.d(210): Error: cannot cast expression `0` of type `int` to `fail_casting1.C`
fail_compilation/fail_casting1.d(211): Error: cannot cast expression `0` of type `int` to `typeof(null)`
fail_compilation/fail_casting1.d(215): Error: cannot cast expression `i` of type `int` to `int delegate()`
fail_compilation/fail_casting1.d(216): Error: cannot cast expression `i` of type `int` to `int[]`
fail_compilation/fail_casting1.d(217): Error: cannot cast expression `i` of type `int` to `int[1]`
fail_compilation/fail_casting1.d(218): Error: cannot cast expression `i` of type `int` to `int[int]`
fail_compilation/fail_casting1.d(219): Error: cannot cast expression `i` of type `int` to `fail_casting1.C`
fail_compilation/fail_casting1.d(220): Error: cannot cast expression `i` of type `int` to `typeof(null)`
fail_compilation/fail_casting1.d(224): Error: cannot cast expression `dg` of type `int delegate()` to `int`
fail_compilation/fail_casting1.d(225): Error: cannot cast expression `da` of type `int[]` to `int`
fail_compilation/fail_casting1.d(226): Error: cannot cast expression `sa` of type `int[1]` to `int`
fail_compilation/fail_casting1.d(227): Error: cannot cast expression `aa` of type `int[int]` to `int`
fail_compilation/fail_casting1.d(228): Error: cannot cast expression `c` of type `fail_casting1.C` to `int`
---
*/
void test4()
{
    { auto x = cast( P) 0; }    // Accept
    { auto x = cast(FP) 0; }    // Accept
    { auto x = cast(DG) 0; }    // Reject (from constfold)
    { auto x = cast(DA) 0; }    // Reject (Bugzilla 11484)
    { auto x = cast(SA) 0; }    // Reject (Bugzilla 11484)
    { auto x = cast(AA) 0; }    // Reject (from constfold)
    { auto x = cast( C) 0; }    // Reject (Bugzilla 11485)
    { auto x = cast( N) 0; }    // Reject (from constfold)

    { auto x = cast( P) i; }    // Accept
    { auto x = cast(FP) i; }    // Accept
    { auto x = cast(DG) i; }    // Reject (from e2ir)
    { auto x = cast(DA) i; }    // Reject (Bugzilla 11484)
    { auto x = cast(SA) i; }    // Reject (Bugzilla 11484)
    { auto x = cast(AA) i; }    // Reject (from e2ir)
    { auto x = cast( C) i; }    // Reject (Bugzilla 11485)
    { auto x = cast( N) i; }    // Reject (from e2ir)

    { auto x = cast(int) p; }   // Accept
    { auto x = cast(int)fp; }   // Accept
    { auto x = cast(int)dg; }   // Reject (from e2ir)
    { auto x = cast(int)da; }   // Reject (Bugzilla 11484)
    { auto x = cast(int)sa; }   // Reject (Bugzilla 11484)
    { auto x = cast(int)aa; }   // Reject (from e2ir)
    { auto x = cast(int) c; }   // Reject (Bugzilla 7472)
    { auto x = cast(int) n; }   // Accept
}

/*
TEST_OUTPUT:
---
fail_compilation/fail_casting1.d(249): Error: cannot cast expression `0` of type `int` to `int[1]`
fail_compilation/fail_casting1.d(250): Error: cannot cast expression `0` of type `int` to `S`
fail_compilation/fail_casting1.d(251): Error: cannot cast expression `i` of type `int` to `int[1]`
fail_compilation/fail_casting1.d(252): Error: cannot cast expression `i` of type `int` to `S`
fail_compilation/fail_casting1.d(253): Error: cannot cast expression `f` of type `double` to `int[1]`
fail_compilation/fail_casting1.d(254): Error: cannot cast expression `f` of type `double` to `S`
fail_compilation/fail_casting1.d(255): Error: cannot cast expression `sa` of type `int[1]` to `int`
fail_compilation/fail_casting1.d(256): Error: cannot cast expression `s` of type `S` to `int`
fail_compilation/fail_casting1.d(257): Error: cannot cast expression `sa` of type `int[1]` to `double`
fail_compilation/fail_casting1.d(258): Error: cannot cast expression `s` of type `S` to `double`
---
*/
void test5()
{
    { auto x = cast(SA) 0; }        // Reject (Bugzilla 14154)
    { auto x = cast( S) 0; }        // Reject (Bugzilla 14154)
    { auto x = cast(SA) i; }        // Reject (Bugzilla 14154)
    { auto x = cast( S) i; }        // Reject (Bugzilla 14154)
    { auto x = cast(SA) f; }        // Reject (Bugzilla 14154)
    { auto x = cast( S) f; }        // Reject (Bugzilla 14154)
    { auto x = cast(int)sa; }       // Reject (Bugzilla 14154)
    { auto x = cast(int) s; }       // Reject (Bugzilla 14154)
    { auto x = cast(double)sa; }    // Reject (Bugzilla 14154)
    { auto x = cast(double) s; }    // Reject (Bugzilla 14154)
}
