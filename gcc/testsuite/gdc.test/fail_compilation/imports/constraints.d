module imports.constraints;

// can be shared between usual and verbose output versions

enum P(T) = true;
enum N(T) = false;

// constraints_func1
void test1(T)(T v) if (N!T);
void test2(T)(T v) if (!P!T);
void test3(T)(T v) if (P!T && N!T);
void test4(T)(T v) if (P!T && N!T && P!T);
void test5(T)(T v) if (N!T || N!T);
void test6(T)(T v) if (N!T || N!T || !P!T);
void test7(T)(T v) if (N!T || P!T && N!T);
void test8(T)(T v) if ((N!T || P!T) && N!T);
void test9(T)(T v) if (!P!T && !N!T);
void test10(T)(T v) if (!N!T && !P!T);
void test11(T)(T v) if (!(!N!T && P!T));
void test12(T)(T v) if (!(N!T || P!T));

// constraints_func2
void test13(T)(T v) if (P!T ? N!T : P!T);    // P!T && N!T || !P!T && P!T
void test14(T)(T v) if (!P!T ? P!T : N!T);
void test15(T)(T v) if (!(P!T ? P!T : N!T)); // (!P!T || !P!T) && (P!T || !N!T)
void test16(T)(T v) if (N!T && P!T || N!T);
void test17(T)(T v) if (N!T && P!T && (P!T || P!T));
void test18(T)(T v) if ((N!T || P!T && N!T) && P!T);
void test19(T)(T v) if ((N!T ? P!T : !P!T) ? P!T : N!T); // (N!T && P!T || !N!T && !P!T) && P!T || (!N!T || !P!T) && (N!T || P!T) && N!T
void test20(T)(T v) if (N!T && (P!T && N!T || N!T));
void test21(T)(T v) if (P!T && (N!T && P!T || N!T));
void test22(T)(T v) if ((!P!T || !P!T && P!T) && (N!T || !P!T));
void test23(T)(T v) if (!P!T || P!T && N!T || !P!T);
void test24(R)(R r) if (__traits(hasMember, R, "stuff"));
int test25(T)(T v) if (N!T);
float test26(T, U)(U u) if (N!U);

// constraints_func3
void overload(T)(T v) if (N!T);
void overload(T)(T v) if (!P!T);
void overload(T)(T v1, T v2) if (N!T);
void overload(T, V)(T v1, V v2) if (N!T || N!V);
void variadic(A, T...)(A a, T v) if (N!int);

// constraints_tmpl
void dummy()() if (false);
void message_nice(T, U)() if (P!T && "message 1" && N!U && "message 2");
void message_ugly(T)(T v) if (!N!T && (T.stringof ~ " must be that") && N!T);
void args(T, U)() if (N!T || N!U);
void lambda(alias pred)() if (N!int);

// constraints_defs
void def(T, int i = 5, alias R)() if (N!T);
void defa(T, U = int)() if (N!T);
void defv(T = bool, int i = 5, Ts...)() if (N!T);

// constraints_aggr
class C
{
    void f(T)(T v) if (P!T && !P!T)
    {}

    void g(this T)() if (N!T)
    {}
}

template S(T) if (N!T)
{
    alias S = T;
}

struct BitFlags(E, bool unsafe = false) if (unsafe || N!E)
{}
