// REQUIRED_ARGS: -o- -d -m64

struct S1 { int v; }
struct S2 { int* p; }
class C { int v; }

#line 6
/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(45): Error: using the result of a comma expression is not allowed
fail_compilation/fail13902.d(32): Error: returning `& x` escapes a reference to local variable `x`
fail_compilation/fail13902.d(33): Error: returning `&s1.v` escapes a reference to local variable `s1`
fail_compilation/fail13902.d(38): Error: returning `& sa1` escapes a reference to local variable `sa1`
fail_compilation/fail13902.d(39): Error: returning `& sa2` escapes a reference to local variable `sa2`
fail_compilation/fail13902.d(40): Error: returning `& x` escapes a reference to local variable `x`
fail_compilation/fail13902.d(41): Error: returning `(& x + 4)` escapes a reference to local variable `x`
fail_compilation/fail13902.d(42): Error: returning `& x + cast(long)x * 4L` escapes a reference to local variable `x`
fail_compilation/fail13902.d(45): Error: returning `& y` escapes a reference to local variable `y`
---
*/
int* testEscape1()
{
    int x, y;
    int[] da1;
    int[][] da2;
    int[1] sa1;
    int[1][1] sa2;
    int* ptr;
    S1 s1;
    S2 s2;
    C  c;

    if (0) return &x;               // VarExp
    if (0) return &s1.v;            // DotVarExp
    if (0) return s2.p;             // no error
    if (0) return &c.v;             // no error
    if (0) return &da1[0];          // no error
    if (0) return &da2[0][0];       // no error
    if (0) return &sa1[0];          // IndexExp
    if (0) return &sa2[0][0];       // IndexExp
    if (0) return &x;
    if (0) return &x + 1;           // optimized to SymOffExp == (& x+4)
    if (0) return &x + x;
  //if (0) return ptr += &x + 1;    // semantic error
    if (0)        ptr -= &x - &y;   // no error
    if (0) return (&x, &y);         // CommaExp

    return null;    // ok
}

#line 49
/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(88): Error: using the result of a comma expression is not allowed
fail_compilation/fail13902.d(75): Error: returning `& x` escapes a reference to parameter `x`
fail_compilation/fail13902.d(76): Error: returning `&s1.v` escapes a reference to parameter `s1`
fail_compilation/fail13902.d(81): Error: returning `& sa1` escapes a reference to parameter `sa1`
fail_compilation/fail13902.d(82): Error: returning `& sa2` escapes a reference to parameter `sa2`
fail_compilation/fail13902.d(83): Error: returning `& x` escapes a reference to parameter `x`
fail_compilation/fail13902.d(84): Error: returning `(& x + 4)` escapes a reference to parameter `x`
fail_compilation/fail13902.d(85): Error: returning `& x + cast(long)x * 4L` escapes a reference to parameter `x`
fail_compilation/fail13902.d(88): Error: returning `& y` escapes a reference to parameter `y`
---
*/
int* testEscape2(
    int x, int y,
    int[] da1,
    int[][] da2,
    int[1] sa1,
    int[1][1] sa2,
    int* ptr,
    S1 s1,
    S2 s2,
    C  c,
)
{
    if (0) return &x;               // VarExp
    if (0) return &s1.v;            // DotVarExp
    if (0) return s2.p;             // no error
    if (0) return &c.v;             // no error
    if (0) return &da1[0];          // no error
    if (0) return &da2[0][0];       // no error
    if (0) return &sa1[0];          // IndexExp
    if (0) return &sa2[0][0];       // IndexExp
    if (0) return &x;
    if (0) return &x + 1;           // optimized to SymOffExp == (& x+4)
    if (0) return  &x + x;
  //if (0) return ptr += &x + 1;    // semantic error
    if (0)        ptr -= &x - &y;   // no error
    if (0) return (&x, &y);         // CommaExp

    return null;    // ok
}

#line 92
/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(123): Error: using the result of a comma expression is not allowed
---
*/
int* testEscape3(
    return ref int x, return ref int y,
    ref int[] da1,
    ref int[][] da2,
    return ref int[1] sa1,
    return ref int[1][1] sa2,
    ref int* ptr,
    return ref S1 s1,
    ref S2 s2,
    ref C  c,
)
{
    if (0) return &x;               // VarExp
    if (0) return &s1.v;            // DotVarExp
    if (0) return s2.p;             // no error
    if (0) return &c.v;             // no error
    if (0) return &da1[0];          // no error
    if (0) return &da2[0][0];       // no error
    if (0) return &sa1[0];          // IndexExp
    if (0) return &sa2[0][0];       // IndexExp
    if (0) return ptr = &x;
    if (0) return ptr = &x + 1;     // optimized to SymOffExp == (& x+4)
    if (0) return ptr = &x + x;
  //if (0) return ptr += &x + 1;    // semantic error
    if (0) return ptr -= &x - &y;   // no error
    if (0) return (&x, &y);         // CommaExp

    return null;    // ok
}

/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(150): Error: returning `cast(int[])sa1` escapes a reference to parameter `sa1`
fail_compilation/fail13902.d(151): Error: returning `cast(int[])sa1` escapes a reference to parameter `sa1`
fail_compilation/fail13902.d(152): Error: returning `sa1[]` escapes a reference to parameter `sa1`
fail_compilation/fail13902.d(155): Error: returning `cast(int[])sa2` escapes a reference to local variable `sa2`
fail_compilation/fail13902.d(156): Error: returning `cast(int[])sa2` escapes a reference to local variable `sa2`
fail_compilation/fail13902.d(157): Error: returning `sa2[]` escapes a reference to local variable `sa2`
fail_compilation/fail13902.d(161): Error: returning `cast(int[])s.sa` escapes a reference to local variable `s`
fail_compilation/fail13902.d(162): Error: returning `cast(int[])s.sa` escapes a reference to local variable `s`
fail_compilation/fail13902.d(163): Error: returning `s.sa[]` escapes a reference to local variable `s`
fail_compilation/fail13902.d(166): Error: escaping reference to stack allocated value returned by `makeSA()`
fail_compilation/fail13902.d(167): Error: escaping reference to stack allocated value returned by `makeSA()`
fail_compilation/fail13902.d(168): Error: escaping reference to stack allocated value returned by `makeSA()`
fail_compilation/fail13902.d(171): Error: escaping reference to stack allocated value returned by `makeS()`
fail_compilation/fail13902.d(172): Error: escaping reference to stack allocated value returned by `makeS()`
fail_compilation/fail13902.d(173): Error: escaping reference to stack allocated value returned by `makeS()`
---
*/
int[] testEscape4(int[3] sa1)       // https://issues.dlang.org/show_bug.cgi?id=9279
{
    if (0) return sa1;                      // error <- no error
    if (0) return cast(int[])sa1;           // error <- no error
    if (0) return sa1[];                    // error

    int[3] sa2;
    if (0) return sa2;                      // error
    if (0) return cast(int[])sa2;           // error
    if (0) return sa2[];                    // error

    struct S { int[3] sa; }
    S s;
    if (0) return s.sa;                     // error <- no error
    if (0) return cast(int[])s.sa;          // error <- no error
    if (0) return s.sa[];                   // error

    int[3] makeSA() { int[3] ret; return ret; }
    if (0) return makeSA();                 // error <- no error
    if (0) return cast(int[])makeSA();      // error <- no error
    if (0) return makeSA()[];               // error <- no error

    S makeS() { S s; return s; }
    if (0) return makeS().sa;               // error <- no error
    if (0) return cast(int[])makeS().sa;    // error <- no error
    if (0) return makeS().sa[];             // error <- no error

    return null;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(201): Error: returning `x` escapes a reference to local variable `x`
fail_compilation/fail13902.d(202): Error: returning `s1.v` escapes a reference to local variable `s1`
fail_compilation/fail13902.d(206): Error: returning `sa1[0]` escapes a reference to local variable `sa1`
fail_compilation/fail13902.d(207): Error: returning `sa2[0][0]` escapes a reference to local variable `sa2`
fail_compilation/fail13902.d(208): Error: returning `x = 1` escapes a reference to local variable `x`
fail_compilation/fail13902.d(209): Error: returning `x += 1` escapes a reference to local variable `x`
fail_compilation/fail13902.d(210): Error: returning `s1.v = 1` escapes a reference to local variable `s1`
fail_compilation/fail13902.d(211): Error: returning `s1.v += 1` escapes a reference to local variable `s1`
---
*/
ref int testEscapeRef1()
{
    int x;
    int[] da1;
    int[][] da2;
    int[1] sa1;
    int[1][1] sa2;
    S1 s1;
    C  c;

    if (0) return x;            // VarExp
    if (0) return s1.v;         // DotVarExp
    if (0) return c.v;          // no error
    if (0) return da1[0];       // no error
    if (0) return da2[0][0];    // no error
    if (0) return sa1[0];       // IndexExp
    if (0) return sa2[0][0];    // IndexExp
    if (0) return x = 1;        // AssignExp
    if (0) return x += 1;       // BinAssignExp
    if (0) return s1.v = 1;     // AssignExp (e1 is DotVarExp)
    if (0) return s1.v += 1;    // BinAssignExp (e1 is DotVarExp)

    static int g;
    return g;       // ok
}

/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(240): Error: returning `x` escapes a reference to parameter `x`
fail_compilation/fail13902.d(241): Error: returning `s1.v` escapes a reference to parameter `s1`
fail_compilation/fail13902.d(245): Error: returning `sa1[0]` escapes a reference to parameter `sa1`
fail_compilation/fail13902.d(246): Error: returning `sa2[0][0]` escapes a reference to parameter `sa2`
fail_compilation/fail13902.d(247): Error: returning `x = 1` escapes a reference to parameter `x`
fail_compilation/fail13902.d(248): Error: returning `x += 1` escapes a reference to parameter `x`
fail_compilation/fail13902.d(249): Error: returning `s1.v = 1` escapes a reference to parameter `s1`
fail_compilation/fail13902.d(250): Error: returning `s1.v += 1` escapes a reference to parameter `s1`
---
*/
ref int testEscapeRef2(
    int x,
    int[] da1,
    int[][] da2,
    int[1] sa1,
    int[1][1] sa2,
    S1 s1,
    C  c,
)
{
    if (0) return x;            // VarExp
    if (0) return s1.v;         // DotVarExp
    if (0) return c.v;          // no error
    if (0) return da1[0];       // no error
    if (0) return da2[0][0];    // no error
    if (0) return sa1[0];       // IndexExp
    if (0) return sa2[0][0];    // IndexExp
    if (0) return x = 1;        // AssignExp
    if (0) return x += 1;       // BinAssignExp
    if (0) return s1.v = 1;     // AssignExp (e1 is DotVarExp)
    if (0) return s1.v += 1;    // BinAssignExp (e1 is DotVarExp)

    static int g;
    return g;       // ok
}

/*
TEST_OUTPUT:
---
---
*/
ref int testEscapeRef2(
    return ref int x,
    ref int[] da1,
    ref int[][] da2,
    return ref int[1] sa1,
    return ref int[1][1] sa2,
    return ref S1 s1,
    ref C  c,
)
{
    if (0) return x;            // VarExp
    if (0) return s1.v;         // DotVarExp
    if (0) return c.v;          // no error
    if (0) return da1[0];       // no error
    if (0) return da2[0][0];    // no error
    if (0) return sa1[0];       // IndexExp
    if (0) return sa2[0][0];    // IndexExp
    if (0) return x = 1;        // AssignExp
    if (0) return x += 1;       // BinAssignExp
    if (0) return s1.v = 1;     // AssignExp (e1 is DotVarExp)
    if (0) return s1.v += 1;    // BinAssignExp (e1 is DotVarExp)

    static int g;
    return g;       // ok
}

/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(294): Error: returning `[& x]` escapes a reference to local variable `x`
fail_compilation/fail13902.d(295): Error: returning `[& x]` escapes a reference to local variable `x`
---
*/
int*[]  testArrayLiteral1() { int x; return [&x]; }
int*[1] testArrayLiteral2() { int x; return [&x]; }

/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(304): Error: returning `S2(& x)` escapes a reference to local variable `x`
fail_compilation/fail13902.d(305): Error: returning `new S2(& x)` escapes a reference to local variable `x`
---
*/
S2  testStructLiteral1() { int x; return     S2(&x); }
S2* testStructLiteral2() { int x; return new S2(&x); }

/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(314): Error: returning `sa[]` escapes a reference to local variable `sa`
fail_compilation/fail13902.d(315): Error: returning `sa[cast(ulong)n..2][1..2]` escapes a reference to local variable `sa`
---
*/
int[] testSlice1() { int[3] sa; return sa[]; }
int[] testSlice2() { int[3] sa; int n; return sa[n..2][1..2]; }

/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(324): Error: returning `vda[0]` escapes a reference to variadic parameter `vda`
fail_compilation/fail13902.d(325): Error: returning `vda[]` escapes a reference to variadic parameter `vda`
---
*/
ref int testDynamicArrayVariadic1(int[] vda...) { return vda[0]; }
@safe int[]   testDynamicArrayVariadic2(int[] vda...) { return vda[]; }
int[3]  testDynamicArrayVariadic3(int[] vda...) { return vda[0..3]; }   // no error

/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(335): Error: returning `vsa[0]` escapes a reference to parameter `vsa`
fail_compilation/fail13902.d(336): Error: returning `vsa[]` escapes a reference to parameter `vsa`
---
*/
ref int testStaticArrayVariadic1(int[3] vsa...) { return vsa[0]; }
int[]   testStaticArrayVariadic2(int[3] vsa...) { return vsa[]; }
int[3]  testStaticArrayVariadic3(int[3] vsa...) { return vsa[0..3]; }   // no error

/*
TEST_OUTPUT:
---
fail_compilation/fail13902.d(355): Error: returning `match(st)` escapes a reference to local variable `st`
---
*/

// This was reduced from a `static assert(!__traits(compiles, {...}))` test in `std.sumtype`
// which was asserting that matchers couldn't escape sumtype members.
// This should give an error even without `@safe` or `-preview=dip1000`

int* match(return ref int i) { return &i; }

int* escape()
{
    int st;
    return match(st);
}
