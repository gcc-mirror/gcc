import std.stdio;

struct S {
    int x = 3;
    void fun(T)(T x) { writefln("S.fun(%s)(%s)",typeid(T),x); this.x += x; }
}

class Tst(TST, int v = 2) {
    int x = 3;
    int z = 4;

    final private void proc(int x) { writefln("proc(%s) -> %s",x,this.x); }
    void fun(T)(T x) { writefln("fun(%s)(%s) -> %s",typeid(T),x,this.x);}
    void fun()() { writefln("fun()() -> %s",this.x); }
    void fun2()() { writefln("fun2"); }

    class Inner {
        int y = 99;
        Tst outer;
        void f3() { z = 55; }
        // Make sure the correct this-ptr is used
        void f1() { writefln("Inner.f1"); proc(-11); outer.proc(-11); }
        void f2() { writefln("Inner.f2"); fun(-17); outer.fun(-17); }
    }
    Inner inner;

    this() {
        inner = new Inner;
        inner.outer = this;
    }

    void callInnerf1() { writefln("callInnerf1"); inner.f1(); }
    void callInnerf2() { writefln("callInnerf2"); inner.f2(); }


//
    void opAdd(T)(T x) { this.x += x; writefln("opAdd(%s)",x); }
    void opPos()() { writefln("opPos()"); }
    //void opPos() { writefln("xxx"); }
    void opIndex(T)(T x) { writefln("opIndex[%s]",x); }

    void opIndex(A,B,C)(A a, B b, C c) {
        writefln("opIndex[(%s)%s,(%s)%s,(%s)%s]",typeid(A),a,
                 typeid(B),b,typeid(C),c);
    }


    static if (v > 1) {
        void opCall(A = int, B = float)(A a = 1, B b = 8.2) { writefln("opCall(%s,%s)",a,b); this.x++; }

    }
    void opSlice(A,B)(A a, B b) { writefln("opSlice(%s,%s)",a,b); }
    void opSlice()() { writefln("opSlice()"); }

    void opIndexAssign(A,B)(A a, B b) {
        writefln("opIndexAssign((%s)%s,(%s)%s)",typeid(A),a,typeid(B),b);
    }

    void opSliceAssign(A,B,C)(A a, B b, C c) {
        writefln("opSliceAssign(%s,%s,%s)",a,b,c);
    }

    bool opEquals(A)(A x) { writefln("opEquals((%s))",typeid(A));return true; }

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
    t[1i] = 5;
    t[-4.5..7i] = "hello";
    t == t;
    auto b = t != t; // without assignment -> "! has no effect in expression"
    t == u;
    u == t;
    u == u;
    b = u != u;
    foreach(int i;t) {
        writefln("%s",i);
    }
    foreach(double i;t) {
        writefln("%s",i);
    }

}
