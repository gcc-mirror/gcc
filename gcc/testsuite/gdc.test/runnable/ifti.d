extern (C) int printf(const scope char*, ...);

struct S {
    int x = 3;
    void fun(T)(T x) { printf("S.fun(%s)(%d)\n", T.stringof.ptr, x); this.x += x; }
}

class Tst(TST, int v = 2) {
    int x = 3;
    int z = 4;

    final private void proc(int x) { printf("proc(%d) -> %d\n", x, this.x); }
    void fun(T)(T x) { printf("fun(%s)(%d) -> %d\n", T.stringof.ptr, x, this.x);}
    void fun()() { printf("fun()() -> %d\n", this.x); }
    void fun2()() { printf("fun2\n"); }

    class Inner {
        int y = 99;
        Tst outer;
        void f3() { z = 55; }
        // Make sure the correct this-ptr is used
        void f1() { printf("Inner.f1\n"); proc(-11); outer.proc(-11); }
        void f2() { printf("Inner.f2\n"); fun(-17); outer.fun(-17); }
    }
    Inner inner;

    this() {
        inner = new Inner;
        inner.outer = this;
    }

    void callInnerf1() { printf("callInnerf1\n"); inner.f1(); }
    void callInnerf2() { printf("callInnerf2\n"); inner.f2(); }


//
    void opBinary(string op : "+", T)(T x) { this.x += x; printf("opAdd(%d)\n", x); }
    void opUnary(string op : "+")() { printf("opPos()\n"); }
    //void opPos() { printf("xxx"); }
    void opIndex(T)(T x) { printf("opIndex[%d]\n",x); }

    void opIndex(A,B,C)(A a, B b, C c) {
        printf("opIndex[(%s) %d, (%s) %d, (%s) %d]\n", A.stringof.ptr, a,
                 B.stringof.ptr,b,C.stringof.ptr,c);
    }

    static if (v > 1) {
        void opCall(A = int, B = float)(A a = 1, B b = 8.2) { printf("opCall(%d, %d)\n",a,b); this.x++; }
    }
    void opSlice(A,B)(A a, B b) { printf("opSlice(%d, %d)\n",a,b); }
    void opSlice()() { printf("opSlice()\n"); }

    void opIndexAssign(A,B)(A a, B b) {
        printf("opIndexAssign((%s) %d, (%s) %d)\n", A.stringof.ptr, a, B.stringof.ptr, b);
    }

    void opSliceAssign(A,B,C)(A a, B b, C c) {
        printf("opSliceAssign(%.s, %d, %d)\n", a.length, a.ptr, b, c);
    }

    bool opEquals(A)(A x) { printf("opEquals((%s))\n", A.stringof.ptr); return true; }

    int opApply(T)(int delegate(ref T)dg) {
        for (int i = 0; i < 5; i++) {
            T d = cast(T)(i+0.1);
            if (auto result = dg(d))
                return result;
        }
        return 0;
    }
}

class Y : Tst!(float) {}

// https://issues.dlang.org/show_bug.cgi?id=24731
void test24731()
{
    static int solve(size_t N)(ref double[N+1][N])
    {
        return N;
    }

    double[3][2] m;
    assert(solve(m) == 2);
    assert(solve!2(m) == 2);
}


void main() {
    Tst!(int) t = new Tst!(int);
    Y u = new Y;
    S s;
    t.x = 7;
    t.proc(5);
    t.fun(5);
    t.fun();
    t.callInnerf1();
    t.callInnerf2();
    u.fun(5);
    u.fun();
    u.callInnerf1();
    u.callInnerf2();
    s.fun(5);
    t.fun2();

    +t;
    t+5;
    t[55];
    t[1,2,3.0];
    u[1,2,3.0];
    t(1,2.5);
    t(2);
    t();
    t[];
    t[1..2];
    u[1..2.5];
    t == t;
    auto b = t != t; // without assignment -> "! has no effect in expression"
    t == u;
    u == t;
    u == u;
    b = u != u;
    foreach(int i;t) {
        printf("%d\n", i);
    }
    foreach(double i;t) {
        printf("%g\n", i);
    }

    test24731();
}
