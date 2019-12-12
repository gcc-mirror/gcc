// PERMUTE_ARGS:

class C {}
class D : C {}

void dynamicarrays()
{
    C[] a;
    D[] b;
    const(C)[] c;
    const(D)[] d;
    immutable(C)[] e;
    immutable(D)[] f;

    static assert( __traits(compiles, a = a));
    static assert(!__traits(compiles, a = b));
    static assert(!__traits(compiles, a = c));
    static assert(!__traits(compiles, a = d));
    static assert(!__traits(compiles, a = e));
    static assert(!__traits(compiles, a = f));

    static assert(!__traits(compiles, b = a));
    static assert( __traits(compiles, b = b));
    static assert(!__traits(compiles, b = c));
    static assert(!__traits(compiles, b = d));
    static assert(!__traits(compiles, b = e));
    static assert(!__traits(compiles, b = f));

    static assert( __traits(compiles, c = a));
    static assert( __traits(compiles, c = b));
    static assert( __traits(compiles, c = c));
    static assert( __traits(compiles, c = d));
    static assert( __traits(compiles, c = e));
    static assert( __traits(compiles, c = f));

    static assert(!__traits(compiles, d = a));
    static assert( __traits(compiles, d = b));
    static assert(!__traits(compiles, d = c));
    static assert( __traits(compiles, d = d));
    static assert(!__traits(compiles, d = e));
    static assert( __traits(compiles, d = f));

    static assert(!__traits(compiles, e = a));
    static assert(!__traits(compiles, e = b));
    static assert(!__traits(compiles, e = c));
    static assert(!__traits(compiles, e = d));
    static assert( __traits(compiles, e = e));
    static assert( __traits(compiles, e = f));

    static assert(!__traits(compiles, f = a));
    static assert(!__traits(compiles, f = b));
    static assert(!__traits(compiles, f = c));
    static assert(!__traits(compiles, f = d));
    static assert(!__traits(compiles, f = e));
    static assert( __traits(compiles, f = f));
}


void statictodynamicarrays()
{
    C[] a;
    D[] b;
    const(C)[] c;
    const(D)[] d;
    immutable(C)[] e;
    immutable(D)[] f;

    C[1] sa;
    D[1] sb;
    const(C)[1] sc = void;
    const(D)[1] sd = void;
    immutable(C)[1] se = void;
    immutable(D)[1] sf = void;

    static assert( __traits(compiles, a = sa));
    static assert(!__traits(compiles, a = sb));
    static assert(!__traits(compiles, a = sc));
    static assert(!__traits(compiles, a = sd));
    static assert(!__traits(compiles, a = se));
    static assert(!__traits(compiles, a = sf));

    static assert(!__traits(compiles, b = sa));
    static assert( __traits(compiles, b = sb));
    static assert(!__traits(compiles, b = sc));
    static assert(!__traits(compiles, b = sd));
    static assert(!__traits(compiles, b = se));
    static assert(!__traits(compiles, b = sf));

    static assert( __traits(compiles, c = sa));
    static assert( __traits(compiles, c = sb));
    static assert( __traits(compiles, c = sc));
    static assert( __traits(compiles, c = sd));
    static assert( __traits(compiles, c = se));
    static assert( __traits(compiles, c = sf));

    static assert(!__traits(compiles, d = sa));
    static assert( __traits(compiles, d = sb));
    static assert(!__traits(compiles, d = sc));
    static assert( __traits(compiles, d = sd));
    static assert(!__traits(compiles, d = se));
    static assert( __traits(compiles, d = sf));

    static assert(!__traits(compiles, e = sa));
    static assert(!__traits(compiles, e = sb));
    static assert(!__traits(compiles, e = sc));
    static assert(!__traits(compiles, e = sd));
    static assert( __traits(compiles, e = se));
    static assert( __traits(compiles, e = sf));

    static assert(!__traits(compiles, f = sa));
    static assert(!__traits(compiles, f = sb));
    static assert(!__traits(compiles, f = sc));
    static assert(!__traits(compiles, f = sd));
    static assert(!__traits(compiles, f = se));
    static assert( __traits(compiles, f = sf));
}

void staticarrays()
{
    C[1] sa;
    D[1] sb;

    const(C)[1] sc = sa;
    const(D)[1] sd = sb;

    sa = sb;
    static assert(!__traits(compiles, sb = sa));
}

void main() {}
