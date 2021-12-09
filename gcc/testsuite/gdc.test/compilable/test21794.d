// https://issues.dlang.org/show_bug.cgi?id=21794
/*
TEST_OUTPUT:
---
0
0u
0L
0LU
0.0F
0.0
0.0L
---
*/

bool fun(void* p) {
    const x = cast(ulong)p;
    return 1;
}

static assert(fun(null));

T fun2(T)(void* p) {
    const x = cast(T)p;
    return x;
}

// These were an error before, they were returning a NullExp instead of IntegerExp/RealExp

static assert(fun2!int(null)    == 0);
static assert(fun2!uint(null)   == 0);
static assert(fun2!long(null)   == 0);
static assert(fun2!ulong(null)  == 0);
static assert(fun2!float(null)  == 0);
static assert(fun2!double(null) == 0);
static assert(fun2!real(null)   == 0);

// These were printing 'null' instead of the corresponding number

const i = cast(int)null;
const ui = cast(uint)null;
const l = cast(long)null;
const ul = cast(ulong)null;
const f = cast(float)null;
const d = cast(double)null;
const r = cast(real)null;
pragma(msg, i);
pragma(msg, ui);
pragma(msg, l);
pragma(msg, ul);
pragma(msg, f);
pragma(msg, d);
pragma(msg, r);
