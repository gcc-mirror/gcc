/*
REQUIRED_ARGS:
TEST_OUTPUT:
---
fail_compilation/fail_scope.d(29): Deprecation: scope variable `da` may not be returned
fail_compilation/fail_scope.d(31): Deprecation: scope variable `o` may not be returned
fail_compilation/fail_scope.d(32): Deprecation: scope variable `dg` may not be returned
fail_compilation/fail_scope.d(34): Deprecation: scope variable `da` may not be returned
fail_compilation/fail_scope.d(36): Deprecation: scope variable `o` may not be returned
fail_compilation/fail_scope.d(37): Deprecation: scope variable `dg` may not be returned
fail_compilation/fail_scope.d(39): Deprecation: scope variable `p` may not be returned
fail_compilation/fail_scope.d(44): Error: returning `cast(char[])string` escapes a reference to local variable `string`
fail_compilation/fail_scope.d(62): Error: returning `s.bar()` escapes a reference to local variable `s`
fail_compilation/fail_scope.d(73): Error: `fail_scope.foo8` called with argument types `(int)` matches both:
fail_compilation/fail_scope.d(67):     `fail_scope.foo8(ref int x)`
and:
fail_compilation/fail_scope.d(68):     `fail_scope.foo8(return ref int x)`
fail_compilation/fail_scope.d(81): Error: returning `& string` escapes a reference to local variable `string`
fail_compilation/fail_scope.d(91): Error: returning `cast(int[])a` escapes a reference to local variable `a`
fail_compilation/fail_scope.d(99): Error: returning `cast(int[])a` escapes a reference to local variable `a`
fail_compilation/fail_scope.d(107): Deprecation: escaping reference to outer local variable `x`
fail_compilation/fail_scope.d(126): Error: returning `s.bar()` escapes a reference to local variable `s`
fail_compilation/fail_scope.d(136): Error: returning `foo16226(i)` escapes a reference to local variable `i`
---
*/

alias int delegate() dg_t;

int[]  checkEscapeScope1(scope int[]  da) @safe { return da; }
int[3] checkEscapeScope2(scope int[3] sa) @safe { return sa; }
Object checkEscapeScope3(scope Object o)  @safe { return o;  }
dg_t   checkEscapeScope4(scope dg_t   dg) @safe { return dg; }

int[]  checkEscapeScope1() @safe { scope int[]  da = [];           return da; }
int[3] checkEscapeScope2() @safe { scope int[3] sa = [1,2,3];      return sa; }
Object checkEscapeScope3() @safe { scope Object  o = new Object;   return o;  }   // same with fail7294.d
dg_t   checkEscapeScope4() @safe { scope dg_t   dg = () => 1;      return dg; }

int* test(scope int* p) @safe { return p; }

char[] foo140()
{
    char[4] string = "abcd";
    return string;
}

/************/

struct S
{
    int x;

    ref int bar() return
    {
        return x;
    }
}

ref int test()
{
    S s;
    return s.bar();
}

/************/

ref int foo8(ref int x);
ref int foo8(return ref int x);

void testover()
{
    int x;
    foo8(x);
}

/************/

char* fail141()
{
    char[4] string = "abcd";
    return string.ptr;
}

/************/

int[] test1313b()
out{}
do
{
    int[2] a;
    return a;
}

int[] test1313a()
//out{}
do
{
    int[2] a;
    return a;
}

/******************/
// https://issues.dlang.org/show_bug.cgi?id=15192

ref int fun15192(ref int x) @safe
{
    ref int bar(){ return x; }
    return bar();
}

ref int fun15192_2(return ref int x) @safe
{
    ref int bar(){ return x; }
    return bar();
}

/**************************/
// https://issues.dlang.org/show_bug.cgi?id=15193

ref int foo15193()@safe{
    struct S{
        int x;
        ref int bar() { return x; }
    }
    S s;
    return s.bar();
}


/*****************************/
// https://issues.dlang.org/show_bug.cgi?id=16226

ref int test16226() @safe
{
    int i;
    return foo16226(i);
}


ref foo16226(ref int bar) @safe
{
    return bar;
}
