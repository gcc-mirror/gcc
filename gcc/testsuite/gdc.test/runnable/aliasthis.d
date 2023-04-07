/*
TEST_OUTPUT:
---
runnable/aliasthis.d(103): Deprecation: alias this for classes/interfaces is deprecated
runnable/aliasthis.d(291): Deprecation: alias this for classes/interfaces is deprecated
runnable/aliasthis.d(292): Deprecation: alias this for classes/interfaces is deprecated
runnable/aliasthis.d(294): Deprecation: alias this for classes/interfaces is deprecated
runnable/aliasthis.d(465): Deprecation: alias this for classes/interfaces is deprecated
runnable/aliasthis.d(466): Deprecation: alias this for classes/interfaces is deprecated
runnable/aliasthis.d(477): Deprecation: alias this for classes/interfaces is deprecated
runnable/aliasthis.d(1013): Deprecation: alias this for classes/interfaces is deprecated
false
runnable/aliasthis.d(2100): Deprecation: alias this for classes/interfaces is deprecated
[] = int
[] = string
[0] = int
[1] = string
[] = string
[] = int
[1] = string
[0] = int
runnable/aliasthis.d(741): Deprecation: alias this for classes/interfaces is deprecated
---

RUN_OUTPUT:
---
1 1.1
ctor
cpctor
dtor
cpctor
dtor
dtor
Success
---
*/

extern (C) int printf(const(char*) fmt, ...);
import core.vararg;

struct Tup(T...)
{
    T field;
    alias field this;

    bool opEquals(const Tup rhs) const
    {
        foreach (i, _; T)
            if (field[i] != rhs.field[i])
                return false;
        return true;
    }
}

Tup!T tup(T...)(T fields)
{
    return typeof(return)(fields);
}

template Seq(T...)
{
    alias T Seq;
}

/**********************************************/

struct S
{
    int x;
    alias x this;
}

int foo(int i)
{
    return i * 2;
}

void test1()
{
    S s;
    s.x = 7;
    int i = -s;
    assert(i == -7);

    i = s + 8;
    assert(i == 15);

    i = s + s;
    assert(i == 14);

    i = 9 + s;
    assert(i == 16);

    i = foo(s);
    assert(i == 14);
}

/**********************************************/

class C
{
    int x;
    alias x this;
}

void test2()
{
    C s = new C();
    s.x = 7;
    int i = -s;
    assert(i == -7);

    i = s + 8;
    assert(i == 15);

    i = s + s;
    assert(i == 14);

    i = 9 + s;
    assert(i == 16);

    i = foo(s);
    assert(i == 14);
}

/**********************************************/

void test3()
{
    Tup!(int, double) t;
    t[0] = 1;
    t[1] = 1.1;
    assert(t[0] == 1);
    assert(t[1] == 1.1);
    printf("%d %g\n", t[0], t[1]);
}

/**********************************************/

struct Iter
{
    bool empty() { return true; }
    void popFront() { }
    ref Tup!(int, int) front() { return *new Tup!(int, int); }
    ref Iter opSlice() return { return this; }
}

void test4()
{
    foreach (a; Iter()) { }
}

/**********************************************/

void test5()
{
    static struct Double1 {
        double val = 1;
        alias val this;
    }
    static Double1 x() { return Double1(); }
    x()++;
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=4617

struct S4617
{
    struct F
    {
        int  square(int  n) { return n*n; }
        real square(real n) { return n*n; }
    }
    F forward;

    alias forward this;

    alias forward.square sqr;    // okay

    int field;
    void mfunc();
    template Templ(){}
    void tfunc()(){}
}

template Id4617(alias k) { alias k Id4617; }

void test4617a()
{
    alias Id4617!(S4617.square) test1;            //NG
    alias Id4617!(S4617.forward.square) test2;    //OK

    alias Id4617!(S4617.sqr) test3;               //okay

    static assert(__traits(isSame, S4617.square, S4617.forward.square));
}

void test4617b()
{
    static struct Sub(T)
    {
        T value;
        @property ref inout(T) payload() inout { return value; }
        alias payload this;
    }

    alias Id4617!(S4617.field) S_field;
    alias Id4617!(S4617.mfunc) S_mfunc;
    alias Id4617!(S4617.Templ) S_Templ;
    alias Id4617!(S4617.tfunc) S_tfunc;

    alias Sub!S4617 T4617;
    alias Id4617!(T4617.field) R_field;
    alias Id4617!(T4617.mfunc) R_mfunc;
    alias Id4617!(T4617.Templ) R_Templ;
    alias Id4617!(T4617.tfunc) R_tfunc;
    static assert(__traits(isSame, R_field, S_field));
    static assert(__traits(isSame, R_mfunc, S_mfunc));
    static assert(__traits(isSame, R_Templ, S_Templ));
    static assert(__traits(isSame, R_tfunc, S_tfunc));

    alias Id4617!(T4617.square) R_sqr;
    static assert(__traits(isSame, R_sqr, S4617.forward.square));
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=4773

void test4773()
{
    struct Rebindable
    {
        Object obj;
        @property const(Object) get(){ return obj; }
        alias get this;
    }

    Rebindable r;
    if (r) assert(0);
    r.obj = new Object;
    if (!r) assert(0);
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=5188

void test5188()
{
    struct S
    {
        int v = 10;
        alias v this;
    }

    S s;
    assert(s <= 20);
    assert(s != 14);
}

/***********************************************/

struct Foo {
  void opIndexAssign(int x, size_t i) {
    val = x;
  }
  void opSliceAssign(int x, size_t a, size_t b) {
    val = x;
  }
  int val;
}

struct Bar {
   Foo foo;
   alias foo this;
}

void test6() {
   Bar b;
   b[0] = 1;
   assert(b.val == 1);
   b[0 .. 1] = 2;
   assert(b.val == 2);
}

/**********************************************/
// recursive alias this detection

class C0 {}

class C1 { C2 c; alias c this; }
class C2 { C1 c; alias c this; }

class C3 { C2 c; alias c this; }

struct S0 {}

struct S1 { S2* ps; @property ref get(){return *ps;} alias get this; }
struct S2 { S1* ps; @property ref get(){return *ps;} alias get this; }

struct S3 { S2* ps; @property ref get(){return *ps;} alias get this; }

struct S4 { S5* ps; @property ref get(){return *ps;} alias get this; }
struct S5 { S4* ps; @property ref get(){return *ps;} alias get this; }

struct S6 { S5* ps; @property ref get(){return *ps;} alias get this; }

void test7()
{
    // Able to check a type is implicitly convertible within a finite time.
    static assert(!is(C1 : C0));
    static assert( is(C2 : C1));
    static assert( is(C1 : C2));
    static assert(!is(C3 : C0));
    static assert( is(C3 : C1));
    static assert( is(C3 : C2));

    static assert(!is(S1 : S0));
    static assert( is(S2 : S1));
    static assert( is(S1 : S2));
    static assert(!is(S3 : S0));
    static assert( is(S3 : S1));
    static assert( is(S3 : S2));

    C0 c0;  C1 c1;  C3 c3;
    S0 s0;  S1 s1;  S3 s3;  S4 s4;  S6 s6;

    // Allow merging types that contains alias this recursion.
    static assert( __traits(compiles, c0 is c1));   // typeMerge(c || c) e2->implicitConvTo(t1);
    static assert( __traits(compiles, c0 is c3));   // typeMerge(c || c) e2->implicitConvTo(t1);
    static assert( __traits(compiles, c1 is c0));   // typeMerge(c || c) e1->implicitConvTo(t2);
    static assert( __traits(compiles, c3 is c0));   // typeMerge(c || c) e1->implicitConvTo(t2);
    static assert(!__traits(compiles, s1 is c0));   // typeMerge(c || c) e1
    static assert(!__traits(compiles, s3 is c0));   // typeMerge(c || c) e1
    static assert(!__traits(compiles, c0 is s1));   // typeMerge(c || c) e2
    static assert(!__traits(compiles, c0 is s3));   // typeMerge(c || c) e2

    static assert(!__traits(compiles, s1 is s0));   // typeMerge(s && s) e1
    static assert(!__traits(compiles, s3 is s0));   // typeMerge(s && s) e1
    static assert(!__traits(compiles, s0 is s1));   // typeMerge(s && s) e2
    static assert(!__traits(compiles, s0 is s3));   // typeMerge(s && s) e2
    static assert(!__traits(compiles, s1 is s4));   // typeMerge(s && s) e1 + e2
    static assert(!__traits(compiles, s3 is s6));   // typeMerge(s && s) e1 + e2

    static assert(!__traits(compiles, s1 is 10));   // typeMerge(s || s) e1
    static assert(!__traits(compiles, s3 is 10));   // typeMerge(s || s) e1
    static assert(!__traits(compiles, 10 is s1));   // typeMerge(s || s) e2
    static assert(!__traits(compiles, 10 is s3));   // typeMerge(s || s) e2

    // SliceExp::semantic
    static assert(!__traits(compiles, c1[]));
    static assert(!__traits(compiles, c3[]));
    static assert(!__traits(compiles, s1[]));
    static assert(!__traits(compiles, s3[]));

    // CallExp::semantic
//  static assert(!__traits(compiles, c1()));
//  static assert(!__traits(compiles, c3()));
    static assert(!__traits(compiles, s1()));
    static assert(!__traits(compiles, s3()));

    // AssignExp::semantic
    static assert(!__traits(compiles, { c1[1] = 0; }));
    static assert(!__traits(compiles, { c3[1] = 0; }));
    static assert(!__traits(compiles, { s1[1] = 0; }));
    static assert(!__traits(compiles, { s3[1] = 0; }));
    static assert(!__traits(compiles, { c1[ ] = 0; }));
    static assert(!__traits(compiles, { c3[ ] = 0; }));
    static assert(!__traits(compiles, { s1[ ] = 0; }));
    static assert(!__traits(compiles, { s3[ ] = 0; }));

    // UnaExp::op_overload
    static assert(!__traits(compiles, +c1[1]));
    static assert(!__traits(compiles, +c3[1]));
    static assert(!__traits(compiles, +s1[1]));
    static assert(!__traits(compiles, +s3[1]));
    static assert(!__traits(compiles, +c1[ ]));
    static assert(!__traits(compiles, +c3[ ]));
    static assert(!__traits(compiles, +s1[ ]));
    static assert(!__traits(compiles, +s3[ ]));
    static assert(!__traits(compiles, +c1));
    static assert(!__traits(compiles, +c3));
    static assert(!__traits(compiles, +s1));
    static assert(!__traits(compiles, +s3));

    // ArrayExp::op_overload
    static assert(!__traits(compiles, c1[1]));
    static assert(!__traits(compiles, c3[1]));
    static assert(!__traits(compiles, s1[1]));
    static assert(!__traits(compiles, s3[1]));

    // BinExp::op_overload
    static assert(!__traits(compiles, c1 + 10));    // e1
    static assert(!__traits(compiles, c3 + 10));    // e1
    static assert(!__traits(compiles, 10 + c1));    // e2
    static assert(!__traits(compiles, 10 + c3));    // e2
    static assert(!__traits(compiles, s1 + 10));    // e1
    static assert(!__traits(compiles, s3 + 10));    // e1
    static assert(!__traits(compiles, 10 + s1));    // e2
    static assert(!__traits(compiles, 10 + s3));    // e2

    // BinExp::compare_overload
    static assert(!__traits(compiles, c1 < 10));    // (Object.opCmp(int) is invalid)
    static assert(!__traits(compiles, c3 < 10));    // (Object.opCmp(int) is invalid)
    static assert(!__traits(compiles, 10 < c1));    // (Object.opCmp(int) is invalid)
    static assert(!__traits(compiles, 10 < c3));    // (Object.opCmp(int) is invalid)
    static assert(!__traits(compiles, s1 < 10));    // e1
    static assert(!__traits(compiles, s3 < 10));    // e1
    static assert(!__traits(compiles, 10 < s1));    // e2
    static assert(!__traits(compiles, 10 < s3));    // e2

    // BinAssignExp::op_overload
    static assert(!__traits(compiles, c1[1] += 1));
    static assert(!__traits(compiles, c3[1] += 1));
    static assert(!__traits(compiles, s1[1] += 1));
    static assert(!__traits(compiles, s3[1] += 1));
    static assert(!__traits(compiles, c1[ ] += 1));
    static assert(!__traits(compiles, c3[ ] += 1));
    static assert(!__traits(compiles, s1[ ] += 1));
    static assert(!__traits(compiles, s3[ ] += 1));
    static assert(!__traits(compiles, c1 += c0));   // e1
    static assert(!__traits(compiles, c3 += c0));   // e1
    static assert(!__traits(compiles, s1 += s0));   // e1
    static assert(!__traits(compiles, s3 += s0));   // e1
    static assert(!__traits(compiles, c0 += c1));   // e2
    static assert(!__traits(compiles, c0 += c3));   // e2
    static assert(!__traits(compiles, s0 += s1));   // e2
    static assert(!__traits(compiles, s0 += s3));   // e2
    static assert(!__traits(compiles, c1 += s1));   // e1 + e2
    static assert(!__traits(compiles, c3 += s3));   // e1 + e2

    // ForeachStatement::inferAggregate
    static assert(!__traits(compiles, { foreach (e; s1){} }));
    static assert(!__traits(compiles, { foreach (e; s3){} }));
    static assert(!__traits(compiles, { foreach (e; c1){} }));
    static assert(!__traits(compiles, { foreach (e; c3){} }));

    // Expression::checkToBoolean
    static assert(!__traits(compiles, { if (s1){} }));
    static assert(!__traits(compiles, { if (s3){} }));

    // SwitchStatement::semantic
    static assert(!__traits(compiles, { switch (c0) { default: } }));
    static assert(!__traits(compiles, { switch (c1) { default: } }));
    static assert(!__traits(compiles, { switch (c3) { default: } }));

    // https://issues.dlang.org/show_bug.cgi?id=12537: function arguments with IFTI
    void eq12537()(Object lhs) {}
    const C0 cc0;
    const C1 cc1;
    const C3 cc3;
    static assert(!__traits(compiles, eq12537(cc0)));
    static assert(!__traits(compiles, eq12537(cc1)));
    static assert(!__traits(compiles, eq12537(cc3)));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11875
// endless recursion in Type::deduceType

struct T11875x(C)
{
    C c;
}
class D11875a { D11875b c; alias c this; }
class D11875b { D11875a c; alias c this; }
static assert(!is(D11875a == D11875b));
static assert( is(T11875x!D11875a == T11875x!D, D) && is(D == D11875a));
static assert(!is(D11875a == T11875x!D, D));    // this used to freeze dmd

// test that types in recursion are still detected
struct T11875y(C)
{
    C c;
    alias c this;
}
class D11875c { T11875y!D11875b c; alias c this; }
static assert(is(D11875c : T11875y!D, D) && is(D == D11875b));

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11930

class BarObj11930 {}

struct Bar11930
{
    BarObj11930 _obj;
    alias _obj this;
}

BarObj11930 getBarObj11930(T)(T t)
{
    static if (is(T unused : BarObj11930))
        return t;
    else
        static assert(false, "Can not get BarObj from " ~ T.stringof);
}

void test11930()
{
    Bar11930 b;
    getBarObj11930(b);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=2781

struct Tuple2781a(T...) {
    T data;
    alias data this;
}

struct Tuple2781b(T) {
    T data;
    alias data this;
}

void test2781()
{
    Tuple2781a!(uint, float) foo;
    foreach(elem; foo) {}

    {
        Tuple2781b!(int[]) bar1;
        foreach(elem; bar1) {}

        Tuple2781b!(int[int]) bar2;
        foreach(key, elem; bar2) {}

        Tuple2781b!(string) bar3;
        foreach(dchar elem; bar3) {}
    }

    {
        Tuple2781b!(int[]) bar1;
        foreach(elem; bar1) goto L1;
    L1:
        ;

        Tuple2781b!(int[int]) bar2;
        foreach(key, elem; bar2) goto L2;
    L2:
        ;

        Tuple2781b!(string) bar3;
        foreach(dchar elem; bar3) goto L3;
    L3:
        ;
    }


    int eval;

    auto t1 = tup(10, "str");
    auto i1 = 0;
    foreach (e; t1)
    {
        pragma(msg, "[] = ", typeof(e));
        static if (is(typeof(e) == int   )) assert(i1 == 0 && e == 10);
        static if (is(typeof(e) == string)) assert(i1 == 1 && e == "str");
        ++i1;
    }

    auto t2 = tup(10, "str");
    foreach (i2, e; t2)
    {
        pragma(msg, "[", cast(int)i2, "] = ", typeof(e));
        static if (is(typeof(e) == int   )) { static assert(i2 == 0); assert(e == 10); }
        static if (is(typeof(e) == string)) { static assert(i2 == 1); assert(e == "str"); }
    }

    auto t3 = tup(10, "str");
    auto i3 = 2;
    foreach_reverse (e; t3)
    {
        --i3;
        pragma(msg, "[] = ", typeof(e));
        static if (is(typeof(e) == int   )) assert(i3 == 0 && e == 10);
        static if (is(typeof(e) == string)) assert(i3 == 1 && e == "str");
    }

    auto t4 = tup(10, "str");
    foreach_reverse (i4, e; t4)
    {
        pragma(msg, "[", cast(int)i4, "] = ", typeof(e));
        static if (is(typeof(e) == int   )) { static assert(i4 == 0); assert(e == 10); }
        static if (is(typeof(e) == string)) { static assert(i4 == 1); assert(e == "str"); }
    }

    eval = 0;
    foreach (i, e; tup(tup((){eval++; return 10;}(), 3.14), tup("str", [1,2])))
    {
        static if (i == 0) assert(e == tup(10, 3.14));
        static if (i == 1) assert(e == tup("str", [1,2]));
    }
    assert(eval == 1);

    eval = 0;
    foreach (i, e; tup((){eval++; return 10;}(), tup(3.14, tup("str", tup([1,2])))))
    {
        static if (i == 0) assert(e == 10);
        static if (i == 1) assert(e == tup(3.14, tup("str", tup([1,2]))));
    }
    assert(eval == 1);
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=6546

void test6546()
{
    class C {}
    class D : C {}

    struct S { C c; alias c this; } // S : C
    struct T { S s; alias s this; } // T : S
    struct U { T t; alias t this; } // U : T

    C c;
    D d;
    S s;
    T t;
    U u;

    assert(c is c);  // OK
    assert(c is d);  // OK
    assert(c is s);  // OK
    assert(c is t);  // OK
    assert(c is u);  // OK

    assert(d is c);  // OK
    assert(d is d);  // OK
    assert(d is s);  // doesn't work
    assert(d is t);  // doesn't work
    assert(d is u);  // doesn't work

    assert(s is c);  // OK
    assert(s is d);  // doesn't work
    assert(s is s);  // OK
    assert(s is t);  // doesn't work
    assert(s is u);  // doesn't work

    assert(t is c);  // OK
    assert(t is d);  // doesn't work
    assert(t is s);  // doesn't work
    assert(t is t);  // OK
    assert(t is u);  // doesn't work

    assert(u is c);  // OK
    assert(u is d);  // doesn't work
    assert(u is s);  // doesn't work
    assert(u is t);  // doesn't work
    assert(u is u);  // OK
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=6736

void test6736()
{
    static struct S1
    {
        struct S2 // must be 8 bytes in size
        {
            uint a, b;
        }
        S2 s2;
        alias s2 this;
    }
    S1 c;
    static assert(!is(typeof(c + c)));
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=2777

struct ArrayWrapper(T) {
    T[] array;
    alias array this;
}

// alias array this
void test2777a()
{
    ArrayWrapper!(uint) foo;
    foo.length = 5;  // Works
    foo[0] = 1;      // Works
    auto e0 = foo[0];  // Works
    auto e4 = foo[$ - 1];  // Error:  undefined identifier __dollar
    auto s01 = foo[0..2];  // Error:  ArrayWrapper!(uint) cannot be sliced with[]
}

// alias tuple this
void test2777b()
{
    auto t = tup(10, 3.14, "str", [1,2]);

    assert(t[$ - 1] == [1,2]);

    auto f1 = t[];
    assert(f1[0] == 10);
    assert(f1[1] == 3.14);
    assert(f1[2] == "str");
    assert(f1[3] == [1,2]);

    auto f2 = t[1..3];
    assert(f2[0] == 3.14);
    assert(f2[1] == "str");
}

/****************************************/
// https://issues.dlang.org/show_bug.cgi?id=2787

struct Base2787
{
    int x;
    void foo() { auto _ = x; }
}

struct Derived2787
{
    Base2787 _base;
    alias _base this;
    int y;
    void bar() { auto _ = x; }
}

/***********************************/
// https://issues.dlang.org/show_bug.cgi?id=5679

void test5679()
{
    class Foo {}

    class Base
    {
        @property Foo getFoo() { return null; }
    }
    class Derived : Base
    {
        alias getFoo this;
    }

    Derived[] dl;
    Derived d = new Derived();
    dl ~= d; // Error: cannot append type alias_test.Base to type Derived[]
}

/***********************************/
// https://issues.dlang.org/show_bug.cgi?id=6508

void test6508()
{
    int x, y;
    Seq!(x, y) = tup(10, 20);
    assert(x == 10);
    assert(y == 20);
}

void test6508x()
{
    static int ctor, cpctor, dtor;

    static struct Tuple(T...)
    {
        T field;
        alias field this;

        this(int)  { ++ctor;   printf("ctor\n");   }
        this(this) { ++cpctor; printf("cpctor\n"); }
        ~this()    { ++dtor;   printf("dtor\n");   }
    }

    {
        alias Tup = Tuple!(int, string);
        auto tup = Tup(1);
        assert(ctor==1 && cpctor==0 && dtor==0);

        auto getVal() { return tup; }
        ref getRef(ref Tup s = tup) { return s; }

        {
            auto n1 = tup[0];
            assert(ctor==1 && cpctor==0 && dtor==0);

            auto n2 = getRef()[0];
            assert(ctor==1 && cpctor==0 && dtor==0);

            auto n3 = getVal()[0];
            assert(ctor==1 && cpctor==1 && dtor==1);
        }

        // bug in DotVarExp::semantic
        {
            typeof(tup.field) vars;
            vars = getVal();
            assert(ctor==1 && cpctor==2 && dtor==2);
        }
    }
    assert(ctor==1 && cpctor==2 && dtor==3);
    assert(ctor + cpctor == dtor);
}

/***********************************/
// https://issues.dlang.org/show_bug.cgi?id=6369

void test6369a()
{
    alias Seq!(int, string) Field;

    auto t1 = Tup!(int, string)(10, "str");
    Field field1 = t1;           // NG -> OK
    assert(field1[0] == 10);
    assert(field1[1] == "str");

    auto t2 = Tup!(int, string)(10, "str");
    Field field2 = t2.field;     // NG -> OK
    assert(field2[0] == 10);
    assert(field2[1] == "str");

    auto t3 = Tup!(int, string)(10, "str");
    Field field3;
    field3 = t3.field;
    assert(field3[0] == 10);
    assert(field3[1] == "str");
}

void test6369b()
{
    auto t = Tup!(Tup!(int, double), string)(tup(10, 3.14), "str");

    Seq!(int, double, string) fs1 = t;
    assert(fs1[0] == 10);
    assert(fs1[1] == 3.14);
    assert(fs1[2] == "str");

    Seq!(Tup!(int, double), string) fs2 = t;
    assert(fs2[0][0] == 10);
    assert(fs2[0][1] == 3.14);
    assert(fs2[0] == tup(10, 3.14));
    assert(fs2[1] == "str");

    Tup!(Tup!(int, double), string) fs3 = t;
    assert(fs3[0][0] == 10);
    assert(fs3[0][1] == 3.14);
    assert(fs3[0] == tup(10, 3.14));
    assert(fs3[1] == "str");
}

void test6369c()
{
    auto t = Tup!(Tup!(int, double), Tup!(string, int[]))(tup(10, 3.14), tup("str", [1,2]));

    Seq!(int, double, string, int[]) fs1 = t;
    assert(fs1[0] == 10);
    assert(fs1[1] == 3.14);
    assert(fs1[2] == "str");
    assert(fs1[3] == [1,2]);

    Seq!(int, double, Tup!(string, int[])) fs2 = t;
    assert(fs2[0] == 10);
    assert(fs2[1] == 3.14);
    assert(fs2[2] == tup("str", [1,2]));

    Seq!(Tup!(int, double), string, int[]) fs3 = t;
    assert(fs3[0] == tup(10, 3.14));
    assert(fs3[0][0] == 10);
    assert(fs3[0][1] == 3.14);
    assert(fs3[1] == "str");
    assert(fs3[2] == [1,2]);
}

void test6369d()
{
    int eval = 0;
    Seq!(int, string) t = tup((){++eval; return 10;}(), "str");
    assert(eval == 1);
    assert(t[0] == 10);
    assert(t[1] == "str");
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=6434

struct Variant6434{}

struct A6434
{
   Variant6434 i;
   alias i this;

   void opDispatch(string name)()
   {
   }
}

void test6434()
{
   A6434 a;
   a.weird; // no property 'weird' for type 'VariantN!(maxSize)'
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=6366

void test6366()
{
    struct Zip
    {
        string str;
        size_t i;
        this(string s)
        {
            str = s;
        }
        @property const bool empty()
        {
            return i == str.length;
        }
        @property Tup!(size_t, char) front()
        {
            return typeof(return)(i, str[i]);
        }
        void popFront()
        {
            ++i;
        }
    }

    foreach (i, c; Zip("hello"))
    {
        switch (i)
        {
            case 0: assert(c == 'h');   break;
            case 1: assert(c == 'e');   break;
            case 2: assert(c == 'l');   break;
            case 3: assert(c == 'l');   break;
            case 4: assert(c == 'o');   break;
            default:assert(0);
        }
    }

    auto range(F...)(F field)
    {
        static struct Range {
            F field;
            bool empty = false;
            Tup!F front() { return typeof(return)(field); }
            void popFront(){ empty = true; }
        }
        return Range(field);
    }

    foreach (i, t; range(10, tup("str", [1,2]))){
        static assert(is(typeof(i) == int));
        static assert(is(typeof(t) == Tup!(string, int[])));
        assert(i == 10);
        assert(t == tup("str", [1,2]));
    }
    auto r1 = range(10, "str", [1,2]);
    auto r2 = range(tup(10, "str"), [1,2]);
    auto r3 = range(10, tup("str", [1,2]));
    auto r4 = range(tup(10, "str", [1,2]));
    alias Seq!(r1, r2, r3, r4) ranges;
    foreach (n, _; ranges)
    {
        foreach (i, s, a; ranges[n]){
            static assert(is(typeof(i) == int));
            static assert(is(typeof(s) == string));
            static assert(is(typeof(a) == int[]));
            assert(i == 10);
            assert(s == "str");
            assert(a == [1,2]);
        }
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6711

void test6711()
{
    struct A { int i; }
    struct B { A a; alias a this; }
    struct C { B b; alias b this; }

    B b;
    with (b)
    {
        i = 42;
    }
    assert(b.i == 42);

    C c;
    with (c)
    {
        i = 42;
    }
    assert(c.i == 42);
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=12161

class A12161
{
    void m() {}
}

class B12161
{
    A12161 a;
    alias a this;
}

void test12161()
{
    B12161 b = new B12161();
    b.a = new A12161();
    with (b)
        m();
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=6759

struct Range
{
    size_t front() { return 0; }
    void popFront() { empty = true; }
    bool empty;
}

struct ARange
{
    Range range;
    alias range this;
}

void test6759()
{
    ARange arange;
    assert(arange.front == 0);
    foreach(e; arange)
    {
        assert(e == 0);
    }
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=6479

struct Memory6479
{
    mixin Wrapper6479!();
}
struct Image6479
{
    Memory6479 sup;
    alias sup this;
}
mixin template Wrapper6479()
{
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=6832

void test6832()
{
    static class Foo { }
    static struct Bar { Foo foo; alias foo this; }
    Bar bar;
    bar = new Foo;          // ok
    assert(bar !is null);   // ng

    struct Int { int n; alias n this; }
    Int a;
    int b;
    auto c = (true ? a : b);    // TODO
    assert(c == a);
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=6928

void test6928()
{
    struct T { int* p; } // p is necessary.
    T tx;

    struct S {
        T get() const { return tx; }
        alias get this;
    }

    immutable(S) s;
    immutable(T) t;
    static assert(is(typeof(1? s:t))); // ok.
    static assert(is(typeof(1? t:s))); // ok.
    static assert(is(typeof(1? s:t)==typeof(1? t:s))); // fail.

    auto x = 1? t:s; // ok.
    auto y = 1? s:t; // compile error.
}

/**********************************************/
// https://issues.dlang.org/show_bug.cgi?id=6929

struct S6929
{
    T6929 get() const { return T6929.init; }
    alias get this;
}
struct T6929
{
    S6929 get() const { return S6929.init; }
    alias get this;
}
void test6929()
{
    T6929 t;
    S6929 s;
    static assert(!is(typeof(1? t:s)));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7136

void test7136()
{
    struct X
    {
        Object get() immutable { return null; }
        alias get this;
    }
    immutable(X) x;
    Object y;
    static assert( is(typeof(1?x:y) == Object));        // fails
    static assert(!is(typeof(1?x:y) == const(Object))); // fails

    struct A
    {
        int[] get() immutable { return null; }
        alias get this;
    }
    immutable(A) a;
    int[] b;
    static assert( is(typeof(1?a:b) == int[]));         // fails
    static assert(!is(typeof(1?a:b) == const(int[])));  // fails
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7731

struct A7731
{
    int a;
}
template Inherit7731(alias X)
{
    X __super;
    alias __super this;
}
struct B7731
{
    mixin Inherit7731!A7731;
    int b;
}

struct PolyPtr7731(X)
{
    X* _payload;
    static if (is(typeof(X.init.__super)))
    {
        alias typeof(X.init.__super) Super;
        @property auto getSuper(){ return PolyPtr7731!Super(&_payload.__super); }
        alias getSuper this;
    }
}
template create7731(X)
{
    PolyPtr7731!X create7731(T...)(T args){
        return PolyPtr7731!X(args);
    }
}

void f7731a(PolyPtr7731!A7731 a) {/*...*/}
void f7731b(PolyPtr7731!B7731 b) {f7731a(b);/*...*/}

void test7731()
{
    auto b = create7731!B7731();
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7808

struct Nullable7808(T)
{
    private T _value;

    this()(T value)
    {
        _value = value;
    }

    @property ref inout(T) get() inout pure @safe
    {
        return _value;
    }
    alias get this;
}

class C7808 {}
struct S7808 { C7808 c; }

void func7808(S7808 s) {}

void test7808()
{
    auto s = Nullable7808!S7808(S7808(new C7808));
    func7808(s);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7945

struct S7945
{
    int v;
    alias v this;
}
void foo7945(ref int n){}

void test7945()
{
    auto s = S7945(1);
    foo7945(s);         // 1.NG -> OK
    s.foo7945();        // 2.OK, ufcs
    foo7945(s.v);       // 3.OK
    s.v.foo7945();      // 4.OK, ufcs
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15674
// alias this on out parameter, consistent with 7945 case

struct S15674
{
    int v;
    alias v this;
}
void foo15674(out int i){ i = 42; }

void test15674()
{
    S15674 s;
    s.v = 1;    foo15674(s);    assert(s.v == 42);
    s.v = 1;    foo15674(s.v);  assert(s.v == 42);
    s.v = 1;    s.foo15674();   assert(s.v == 42);
    s.v = 1;    s.v.foo15674(); assert(s.v == 42);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7979

void test7979()
{
    static struct N
    {
        int val;
        alias val this;
    }
    N n = N(1);

    switch (n)
    {
        case 0:
            assert(0);
        case 1:
            break;
        default:
            assert(0);
    }

    static struct S
    {
        string val;
        alias val this;
    }
    S s = S("b");

    switch (s)
    {
        case "a":
            assert(0);
        case "b":
            break;
        default:
            assert(0);
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7992

struct S7992
{
    int[] arr;
    alias arr this;
}
S7992 func7992(...)
{
    S7992 ret;
    ret.arr.length = _arguments.length;
    return ret;
}
void test7992()
{
    int[] arr;
    assert(arr.length == 0);
    arr ~= func7992(1, 2);  //NG
    //arr = func7992(1, 2); //OK
    assert(arr.length == 2);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8169

void test8169()
{
    static struct ValueImpl
    {
       static immutable(int) getValue()
       {
           return 42;
       }
    }

    static struct ValueUser
    {
       ValueImpl m_valueImpl;
       alias m_valueImpl this;
    }

    static assert(ValueImpl.getValue() == 42); // #0, OK
    static assert(ValueUser.getValue() == 42); // #1, NG -> OK
    static assert(       ValueUser.m_valueImpl .getValue() == 42); // #2, NG -> OK
    static assert(typeof(ValueUser.m_valueImpl).getValue() == 42); // #3, OK
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8735

struct S8735(alias Arg)
{
    alias Arg Val;
    alias Val this;
}

struct Tuple9709(T...)
{
    alias T expand;
    alias expand this;
}

void test8735()
{
    alias S8735!1 S;
    S s;
    int n = s;
    assert(n == 1);

    // https://issues.dlang.org/show_bug.cgi?id=11502
    static void f(int i);
    S8735!f sf;

    // https://issues.dlang.org/show_bug.cgi?id=9709
    alias A = Tuple9709!(1,int,"foo");
    A a;
    //static assert(A[0] == 1);
    static assert(a[0] == 1);
    //static assert(is(A[1] == int));
    //static assert(is(a[1] == int));
    //static assert(A[2] == "foo");
    static assert(a[2] == "foo");
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9174

void test9174()
{
    static struct Foo
    {
        char x;
        alias x this;
    }
    static assert(is(typeof(true ? 'A' : Foo()) == char));
    static assert(is(typeof(true ? Foo() : 100) == int));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9177

struct S9177
{
    int foo(int){ return 0; }
    alias foo this;
}
pragma(msg, is(S9177 : int));

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9858

struct S9858()
{
    @property int get() const
    {
        return 42;
    }
    alias get this;
    void opAssign(int) {}
}
void test9858()
{
    const S9858!() s;
    int i = s;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9873

void test9873()
{
    struct Tup(T...) { T field; alias field this; }

    auto seq1 = Seq!(1, "hi");
    assert(Seq!(1, "hi") == Seq!(1, "hi"));
    assert(seq1          == Seq!(1, "hi"));
    assert(Seq!(1, "hi") == seq1);
    assert(seq1          == seq1);

    auto seq2 = Seq!(2, "hi");
    assert(Seq!(1, "hi") != Seq!(2, "hi"));
    assert(seq2          != Seq!(1, "hi"));
    assert(Seq!(1, "hi") != seq2);
    assert(seq2          != seq1);

    auto tup1 = Tup!(int, string)(1, "hi");
    assert(Seq!(1, "hi") == tup1);
    assert(seq1          == tup1);
    assert(tup1          == Seq!(1, "hi"));
    assert(tup1          == seq1);

    auto tup2 = Tup!(int, string)(2, "hi");
    assert(Seq!(1, "hi") != tup2);
    assert(seq1          != tup2);
    assert(tup2          != Seq!(1, "hi"));
    assert(tup2          != seq1);

    static assert(!__traits(compiles, seq1 == Seq!(1, "hi", [1,2])));
    static assert(!__traits(compiles, tup1 == Seq!(1, "hi", [1,2])));
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10178

void test10178()
{
    struct S { static int count; }
    S s;
    assert((s.tupleof == s.tupleof) == true);
    assert((s.tupleof != s.tupleof) == false);

    S getS()
    {
        S s;
        ++S.count;
        return s;
    }
    assert(getS().tupleof == getS().tupleof);
    assert(S.count == 2);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10179

void test10179()
{
    struct S { static int count; }
    S s;
    static assert(s.tupleof.length == 0);
    s.tupleof = s.tupleof;   // error -> OK

    S getS()
    {
        S s;
        ++S.count;
        return s;
    }
    getS().tupleof = getS().tupleof;
    assert(S.count == 2);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9890

void test9890()
{
    struct RefCounted(T)
    {
        T _payload;

        ref T refCountedPayload()
        {
            return _payload;
        }

        alias refCountedPayload this;
    }

    struct S(int x_)
    {
        alias x_ x;
    }

    alias RefCounted!(S!1) Rs;
    static assert(Rs.x == 1);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10004

void test10004()
{
    static int count = 0;

    static S make(S)()
    {
        ++count;    // necessary to make this function impure
        S s;
        return s;
    }

    struct SX(T...) {
        T field; alias field this;
    }
    alias S = SX!(int, long);
    assert(make!S.field == make!S.field);
    assert(count == 2);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10180

template TypeTuple10180(TL...) { alias TypeTuple10180 = TL; }

template Identity10180(alias T) { alias Identity10180 = T; }

struct Tuple10180(Specs...)
{
    static if (is(Specs))
    {
        alias Types = Specs;
        Types expand;
        alias expand this;
    }
    else
    {
        alias Types = TypeTuple10180!(Specs[0]);
        Types expand;
        mixin("alias Identity10180!(expand[0]) "~Specs[1]~";");

        @property
        ref Tuple10180!(Specs[0]) _Tuple_super()
        {
            return *cast(typeof(return)*) (&expand[0]);
        }
        alias _Tuple_super this;
    }
}

void test10180()
{
    Tuple10180!(int, "a") x;
    auto o1 = x.a.offsetof;     // OK
    auto o2 = x[0].offsetof;    // NG: no property 'offsetof' for type 'int'
    auto o3 = x._Tuple_super[0].offsetof;   // same as above
    assert(o2 == o3);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10456

void test10456()
{
    S10456 s1, s2;
    auto x = s1 == s2;
}

struct S10456
{
    enum E { e };
    alias E this;
    int[] x;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11261

template Tuple11261(Specs...)
{
    struct Tuple11261
    {
        static if (Specs.length != 4)   // anonymous field version
        {
            alias Specs Types;
            Types expand;
            alias expand this;
        }
        else
        {
            alias Seq!(Specs[0], Specs[2]) Types;
            Types expand;
            ref inout(Tuple11261!Types) _Tuple_super() inout @trusted
            {
                return *cast(typeof(return)*) &(expand[0]);
            }
            // This is mostly to make t[n] work.
            alias _Tuple_super this;
        }

        this()(Types values)
        {
            expand[] = values[];
        }
    }
}

interface InputRange11261(E)
{
    @property bool empty();
    @property E front();
    void popFront();

    int opApply(int delegate(E));
    int opApply(int delegate(size_t, E));

}
template InputRangeObject11261(R)
{
    alias typeof(R.init.front()) E;

    class InputRangeObject11261 : InputRange11261!E
    {
        private R _range;

        this(R range) { this._range = range; }

        @property bool empty() { return _range.empty; }
        @property E front() { return _range.front; }
        void popFront() { _range.popFront(); }

        int opApply(int delegate(E) dg) { return 0; }
        int opApply(int delegate(size_t, E) dg) { return 0; }
    }
}

// ------

class Container11261
{
    alias Tuple11261!(string, "key", string, "value") Key;

    InputRange11261!Key opSlice()
    {
        Range r;
        return new InputRangeObject11261!Range(r);
    }
    private struct Range
    {
        enum empty = false;
        auto popFront() {}
        auto front() { return Key("myKey", "myValue"); }
    }
}

void test11261()
{
    auto container = new Container11261();
    foreach (k, v; container)   // map the tuple of container[].front to (k, v)
    {
        static assert(is(typeof(k) == string) && is(typeof(v) == string));
        break;
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11333

alias id11333(a...) = a;

struct Unit11333
{
    enum value = Unit11333.init.tupleof;
    alias value this;
}

void test11333()
{
    void foo() {}

    id11333!() unit;
    unit = unit; // ok
    foo(unit);   // ok

    unit = Unit11333.value; // ok
    foo(Unit11333.value);   // ok

    Unit11333 unit2;
    unit = unit2; // ok <- segfault
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11538

struct NullableRef11538(T)
{
    T* _value;
    inout(T) get() inout { return *_value; }
    alias get this;
}

struct S11538
{
    NullableRef11538!S11538 parent;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11800

struct A11800
{
    B11800 b;
    alias b this;
}

struct B11800
{
    static struct Value {}
    Value value;
    alias value this;

    void foo(ref const B11800 rhs)
    {
    }
}

void test11800()
{
    A11800 a;
    B11800 b;
    b.foo(a);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12008

struct RefCounted12008(T)
{
    struct RefCountedStore
    {
        private struct Impl
        {
            T _payload;
        }

        private void initialize(A...)(auto ref A args)
        {
            import core.memory;
        }

        void ensureInitialized()
        {
            initialize();
        }

    }
    RefCountedStore _refCounted;

    void opAssign(T rhs)
    {
    }

    int refCountedPayload()
    {
        _refCounted.ensureInitialized();
        return 0;
    }

    int refCountedPayload() inout
    {
        return 0;
    }

    alias refCountedPayload this;
}

struct SharedInput12008
{
    Group12008 unused;
}

struct Group12008
{
    RefCounted12008!SharedInput12008 _allGroups;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12038

bool f12038(void* p) { return true; }

struct S12038
{
    @property p() { f12038(&this); }
    alias p this;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13490

struct S13490
{
    int i;
    alias i this;
}

struct T13490
{
    S13490[] a1, a2;
}

void test13490()
{
    T13490 t;

    (true ? t.a1 : t.a2) ~= S13490(1);
    assert(t.a1 == [S13490(1)]);
    assert(t.a2 == []);

    (false ? t.a1 : t.a2) ~= S13490(2);
    assert(t.a1 == [S13490(1)]);
    assert(t.a2 == [S13490(2)]);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11355

struct A11355
{
    static int postblit;
    this(this) { ++postblit; }
}

struct B11355
{
    A11355 a;
    alias a this;
}

B11355 make11355()
{
    return B11355();
}
void test11355()
{
    A11355 a1 = make11355();
    assert(A11355.postblit == 1);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13009

struct T13009
{
    void put(char c) {}
}

struct S13009(bool rev)
{
    T13009 t;

    static if (!rev)
    {
        @property       T13009  getT()       { return t; }
        @property inout(T13009) getT() inout { return t; }
    }
    else
    {
        @property inout(T13009) getT() inout { return t; }
        @property       T13009  getT()       { return t; }
    }

    alias getT this;
}

void test13009()
{
    foreach (bool rev; Seq!(false, true))
    {
        alias S = S13009!rev;

        alias MS   =                    S;
        alias CS   =              const(S);
        alias WS   =        inout(      S);
        alias WCS  =        inout(const S);
        alias SMS  = shared(            S);
        alias SCS  = shared(      const S);
        alias SWS  = shared(inout       S);
        alias SWCS = shared(inout const S);
        alias IS   =          immutable(S);

        alias MSput  = MS .put;
        alias CSput  = CS .put;
        alias WSput  = WS .put;
        alias WCSput = WCS.put;
        static assert(!__traits(compiles, { alias SMSput  = SMS .put; }));
        static assert(!__traits(compiles, { alias SCSput  = SCS .put; }));
        static assert(!__traits(compiles, { alias SWSput  = SWS .put; }));
        static assert(!__traits(compiles, { alias SWCSput = SWCS.put; }));
        alias ISput  = IS .put;
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14806

struct Nullable14806
{
    float get() { return float.nan; }
    alias get this;
}

struct Foo14806(T)
{
    T bar;
    Nullable14806 baz;
}

void test14806()
{
    Foo14806!int a, b;
    assert(a == b);
    // ==> a.tupleof != b.tupleof
    // ==> a.bar != b.bar || a.baz.get() != b.baz.get()

    Foo14806!string c, d;
    assert(c == d);
    // ==> c.tupleof != d.tupleof
    // ==> c.bar != d.bar || c.baz.get() != d.baz.get()
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14948

struct RefCounted14948(T)
{
    struct Impl
    {
        T data;
    }
    Impl* impl;

    @property ref T payload() { return impl.data; }

    alias payload this;
}

struct HTTP14948
{
    struct Impl
    {
    }

    RefCounted14948!Impl p;
}

void test14948()
{
    int[HTTP14948] aa;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15292

struct NullableRef15292(T)
{
    inout(T) get() inout
    {
        assert(false);
    }

    alias get this;
}

struct S15292
{
    NullableRef15292!S15292 n;  // -> no segfault

    /* The field 'n' contains alias this, so to use it for the equality,
     * following helper function is automatically generated in buildXopEquals().
     *
     *  static bool __xopEquals(ref const S15292 p, ref const S15292 q)
     *  {
     *      return p == q;
     *  }
     *
     * In its definition, const(S15292) equality is analyzed. It fails, then
     * the error is gagged.
     */
}

/***************************************************/

struct S19284a { int x; }
struct S19284b
{
    S19284a s;
    alias s this;
    int t;
    void f()
    {
        void wrapped()
        {
            x = 1;
            t = 1;
        }
        wrapped(); // <-- 'x' not found, whereas 's.x' works fine
    }

    void f1()
    {
        x = 2;
    }

    void f2()
    {
        int x;
        void wrapped()
        {
            x = 7;
        }
        wrapped();
        assert(x == 7);
    }

    void f3()
    {
        void wrapped()
        {
            void wrapped2()
            {
                x = 5;
            }
            wrapped2();
        }
        wrapped();
    }
}

void test19284()
{
    S19284b t;

    // nested function modifies alias this
    t.f();
    assert(t.x == 1);
    assert(t.t == 1);

    // member function modifies alias this
    t.f1();
    assert(t.x == 2);

    // nested function does not modify alias this when it is shadowd by a local variable
    t.f2();
    assert(t.x == 2);

    // multiple levels of nesting
    t.f3();
    assert(t.x == 5);
}

// 16633

class Item
{
    alias children this;
    Item[] children;
    void populate()
    {
        children ~= new Item(); // Item is seen as []
        assert(children.length == 1);
    }
}

void test16633()
{
    Item root = new Item();
    root.populate;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13009

struct RefCounted13009_2(T)
{
    ref T refCountedPayload()
    {
        assert(false);
    }

    ref inout(T) refCountedPayload() inout
    {
        assert(false);
    }

    alias refCountedPayload this;
}

struct S13009_2
{
    struct Payload
    {
        int[] data;
    }

    RefCounted13009_2!Payload payload;
    alias X = typeof(payload.data[0]);

    void foo()
    {
        payload.data[0] = 0;
    }
}

/***************************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test4617a();
    test4617b();
    test4773();
    test5188();
    test6();
    test7();
    test2781();
    test6546();
    test6736();
    test2777a();
    test2777b();
    test5679();
    test6508();
    test6508x();
    test6369a();
    test6369b();
    test6369c();
    test6369d();
    test6434();
    test6366();
    test6711();
    test12161();
    test6759();
    test6832();
    test6928();
    test6929();
    test7136();
    test7731();
    test7808();
    test7945();
    test15674();
    test7979();
    test7992();
    test8169();
    test8735();
    test9174();
    test9858();
    test9873();
    test10178();
    test10179();
    test9890();
    test10004();
    test10180();
    test10456();
    test11333();
    test11800();
    test13490();
    test11355();
    test14806();
    test19284();
    test16633();

    printf("Success\n");
    return 0;
}
