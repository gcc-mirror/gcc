// Basic out-of-order test
int f0(T0, T1)(T0 t0, T1 t1)
{
    static assert(is(T0 == int));
    static assert(is(T1 == string));
    return t0;
}

static assert(f0(t1: "a", t0: 10) == 10);

// Default argument at beginning instead of end
int f1(T0, T1)(T0 t0 = 20, T1 t1) { return t0; }
static assert(f1(t1: "a") == 20);

// Two default arguments
int f2(T0, T1)(T0 t0 = 20, T1 t1, T2 t2 = 30) { return t2; }

// Selecting overload based on name
string f3(T)(T x) { return "x"; }
string f3(T)(T y) { return "y"; }
static assert(f3(x: 0) == "x");
static assert(f3(y: 0) == "y");

// Variadic tuple cut short by named argument
int f4(T...)(T x, int y, int z) { assert(y == 30); assert(z == 50); return T.length; }
static assert(f4(10, 10, 10, y: 30, z: 50) == 3);
static assert(f4(10, 10, 30, z: 50) == 2);
