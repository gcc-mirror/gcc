// PERMUTE_ARGS:

module run.module_01;

import core.memory;
import core.exception;
import core.vararg;

extern(C) void* malloc(size_t size);
extern(C) int printf(const char*, ...);

/* ================================ */

struct MyStruct
{
        int i;
}

void test1()
{
        MyStruct inner()
        in{
                assert(1);
        }out (result){
                assert(result.i==1);
        }do{
                MyStruct s;
                s.i = 1;
                return s;
        }
        assert(inner.i==1);
}

/* ================================ */

void foo2()
in{
        assert(0);
}
do{
}

void test2()
{
        try{
                foo2();
        }catch(Error ie){
                return;
        }
        assert(0);
}

/* ================================ */

class MyClass3
{

        private int g() const {
                return 1;
        }

        invariant()
        {
                assert(g()==1);
        }
}

void test3()
{
        MyClass3 c = new MyClass3();
        assert(c);
}

/* ================================ */

class A
{
        int x;

        this(){
            printf("A.this()\n");
            x=3;
        }

        invariant()
        {
            printf("A.invariant\n");
            assert(x>2);
        }
}

class B : A
{
        int y;

        this(){
            printf("B.this()\n");
            y=5;
        }

        invariant()
        {
            printf("B.invariant\n");
            assert(y>4);
        }
}

void test4()
{
        B gc = new B();
}

/* ================================ */

class A5
{

        int x;

        this(){
            printf("A5.this()\n");
            x=3;
        }

        invariant()
        {
            printf("A5.invariant\n");
            assert(x>2);
        }
}

class C5 : A5 { }

class B5 : C5
{

        int y;

        this(){
            printf("B5.this()\n");
            y=5;
        }

        invariant()
        {
            printf("B5.invariant\n");
            assert(y>4);
        }
}

void test5()
{
        B5 gc = new B5();
}

/* ================================ */

void test6()
{
        long a;
        assert(a.max == 0x7FFF_FFFF_FFFF_FFFFL);
        //assert(a.min == 0xFFFF_FFFF_FFFF_FFFFL);
        assert(a.min == 0x8000_0000_0000_0000L);
        assert(a.init == 0);
        assert(a.sizeof == 8);
}

/* ================================ */

int i;

void test7()
{
        assert(run.module_01.i==0);
        run.module_01.i++;
        assert(run.module_01.i==1);
}

/* ================================ */

template mix(){
        int x;
}

mixin .mix;

void test8()
{
}

/* ================================ */

struct A9
{
    B9 next;
}

alias A9* B9;

void test9()
{
    B9 n = new A9;
}

/* ================================ */


struct TestStruct
{
        void add(...)
        {
                TestStruct other = va_arg!TestStruct(_argptr);
                foreach(int value; other)
                {
                        foo();
                }
        }

        void foo()
        {
                assert(left is null);
                bar();
        }

        void bar()
        {
                assert(left is null);
        }

        int opApply(int delegate(ref int val) dg)
        {
                return 0;
        }

        void* left;
}

void test10()
{
        TestStruct t;
        t.foo();
}

/* ================================ */

int status11;

void check(int x){
        status11++;
}

class MyClass11{
        void test(){
                assert(status11==0);
                .check(0);
                assert(status11==1);
                check();
                assert(status11==3);
        }

        void check(){
                assert(status11==1);
                status11+=2;
        }
}

void test11()
{
        MyClass11 c = new MyClass11();
        assert(status11==0);
        c.test();
        assert(status11==3);
        check(0);
        assert(status11==4);
}

/* ================================ */

int status12;

class C12{
        void foo(){
                status12='N';
        }

        static void foo(int x){
                status12=x;
        }
}

void test12()
{
        C12 m = new C12();

        C12.foo('S');
        assert(status12=='S');

        m.foo('s');
        assert(status12=='s');

        m.foo();
        assert(status12=='N');
}

/* ================================ */

void test13()
{
        wstring a="abc";
        wstring b="efg";
        wstring c=a~"d"~b;

        assert(c == "abcdefg");
}

/* ================================ */

void test14()
{
        dstring a="abc";
        dstring b="efg";
        dstring c=a~"d"~b;

        assert(c == "abcdefg");
}

/* ================================ */

class Parent15
{
        void test(int i){
        }

        int opCast(){
                return 0;
        }
}

class Child15 : Parent15
{
}

void test15()
{
        (new Child15()).test(2);
}

/* ================================ */

class Parent16
{
        void test(int i){
        }
}

class Child16 : Parent16
{
        int opCast(){
                return 0;
        }
}

void test16()
{
        (new Child16()).test=2;
}

/* ================================ */

void foo17(){
        class MyClass{
                static int x;
        }
}

void foo17(int i){
        class MyClass{
                static int x;
        }
}

void test17()
{
}

/* ================================ */

void foo18(){
        struct MyStruct{
                static int x;
        }
}

void foo18(int i){
        struct MyStruct{
                static int x;
        }
}

void test18()
{
}

/* ================================ */

class Class19 : Throwable
{
    this() { super(""); }
}

alias Class19 Alias19;

void test19()
{
        try{
                throw new Alias19();
        }catch(Throwable){
                return;
        }
        assert(0);
}

/* ================================ */

public static const uint U20 = (cast(uint)(-1)) >>> 2;

alias uint myType20;
public static const myType20 T20 = (cast(myType20)(-1)) >>> 2;

void test20()
{
        assert(U20 == 0x3FFFFFFF);
        assert(T20 == 0x3FFFFFFF);
}

/* ================================ */

class C21(T1){
        alias T1 type1;
}

class C21(T1, T2){
        alias T1 type1;
        alias .C21!(T2) type2;
}

void test21()
{
        alias C21!(int,long) CT;
        CT c = new CT();
}

/* ================================ */

class AutoClass{
}

void test22()
{
        scope AutoClass ac = new AutoClass();

        with(ac){
        }
}

/* ================================ */

int status23;

class C23{
        ~this(){
                assert(status23==0);
                status23--;
                throw new Exception("error msg");
        }
}

void foo23(){
        assert(status23==0);
        scope C23 ac = new C23();
}

void test23()
{
        try{
                foo23();
        }catch(Throwable){
        }
        assert(status23==-1);
}

/* ================================ */

int status24;

class C24{
        this() scope {
                assert(status24==0);
                status24+=2;
        }
        ~this(){
                assert(status24==2);
                status24--;
                throw new Exception("error msg");
        }
}

void check24(){
        scope C24 ac = new C24();
        throw new Exception("check error");
}

void test24()
{
        assert(status24==0);
        try{
                check24();
        }catch(Throwable){
                assert(status24==1);
                status24-=5;
        }
        assert(status24==-4);
}

/* ================================ */

struct S25{
        S25 opBinary(string op)(int i) if (op == "-") { S25 s; return s; }
}

struct GeomObject{
        S25     mesh;
        int     xlate;
}


void extractTriangles(GeomObject g)
{
    void foobar()
    {
        g.mesh - g.xlate;
        //g.mesh.opBinary!("-")(g.xlate);
    }

    foobar();
}

void test25()
{
}

/* ================================ */

struct S26{
        int i;

        void display(){
                assert(i==10);
        }

        void someFunc(){
                // We never call this function
                void bug(S26[] array){
                        array[0].i = i+1;
                }

                assert(i==10);
                display();
                assert(i==10);
        }
}

void test26()
{
        S26 m;
        m.i = 10;
        assert(m.i==10);
        m.someFunc();
        assert(m.i==10);
}

/* ================================ */

template foo27(T:T[],alias S) {
        string foo(T[] a, T[] b) {
                return a ~ S ~ b;
        }
}

string comma = ", ";
alias foo27!(string,comma).foo catComma;

void test27()
{
        string a = "Heath";
        string b = "Regan";

        assert("Heath, Regan"==catComma(a,b));
}

/* ================================ */

void test28()
{
        assert((new S28!()).i==int.sizeof);
}

struct S28(){
        int i=func28(0).sizeof;
}

int func28(...){
        return 0;
}

/* ================================ */

void test29()
{
        uint u;
        u = 1 << 31;
        assert( u == 0b1000_0000__0000_0000__0000_0000__0000_0000u);
}

/* ================================ */
// Test for FinalizeError - it will be thrown if an Exception is thrown
// during the class object finalization.

int status30;

class C30
{
    this()
    {
        status30++;
    }

    ~this()
    {
        status30--;
        throw new Exception("E2");
    }
}

void test30()
{
    try
    {
        //scope C30 m = new C30();
        // It will insert one more `delete m` for the scope destruction, and it will be
        // called during stack unwinding.
        // Instead use bare memory chunk on stack to construct dummy class instance.
        void[__traits(classInstanceSize, C30)] payload =
            typeid(C30).initializer[];
        C30 m = cast(C30)payload.ptr;
        m.__ctor();

        assert(status30 == 1);

        destroy(m);   // _d_callfinalizer
    }
    catch (Error e) // FinalizeError
    {
        assert(status30 == 0);
        status30--;
    }

    assert(status30 == -1);
}

/* ================================ */

void test31()
{
        string str = "\xF0\x9D\x83\x93"; // utf-8 for U+1D0D3

        int count=0;
        dchar tmp;
        foreach(dchar value ; str){
                tmp=value;
                count++;
        }
        assert(count==1);
        assert(tmp==0x01D0D3);
}

/* ================================ */

union MyUnion32
{
        int i;
        byte b;
}

void test32()
{
        TypeInfo ti = typeid(MyUnion32*);
        assert(!(ti is null));
        assert(ti.tsize==(MyUnion32*).sizeof);
        assert(ti.toString()=="run.module_01.MyUnion32*");
}

/* ================================ */

void test35()
{
        try{
                onOutOfMemoryError();
        }catch(OutOfMemoryError e){
                return;
        }
        assert(0);
}

/* ================================ */

void test36()
{
        try{
                onOutOfMemoryError();
        }catch(OutOfMemoryError e){
                return;
        }
        assert(0);
}

/* ================================ */

struct S37{
        int a;
}

const int i37 = 15;

const S37 s1 = { i37+1 };
const S37 s2 = s1;

void test37()
{
        assert(s1.a == 16);
        assert(s2.a == 16);
}

/* ================================ */

class Foo38
{
        enum MyEnum{
                VALUE_A=1,
        }
}

class Bar38
{
        enum MyEnum{
                VALUE_B=2,
        }
}

void test38()
{
        assert(Foo38.MyEnum.VALUE_A==1);
        assert(Bar38.MyEnum.VALUE_B==2);
}

/* ================================ */

void test39()
{
        bool[] bArray;
        int[] iArray;

        bArray[]=false;

        foreach(int c; iArray){
                assert(0);
        }
}

/* ================================ */

bool checked40;

class Parent40{
        int x;

        void test(){
        }

        invariant()
        {
                assert(!checked40);
                checked40=true;
                // even number
                assert((x&1u)==0);
        }
}

class Child40 : Parent40{
}

class GrandChild40 : Child40{
        this(){
                x=5;
        }
}

void test40()
{
        try{
                assert(!checked40);
                GrandChild40 gc = new GrandChild40();
        }catch(Throwable){
                assert(checked40);
                return;
        }
        assert(0);
}

/* ================================ */

int counter41;

class C41{
        this(){
                printf("this: counter41 = %d\n", counter41);
                assert(counter41==0);
                counter41+=2;
        }
}

void test41()
{
        C41 c;
        assert(counter41==0);
        c = new C41();
        assert(counter41==2);
}

/* ================================ */

struct S42{
        int y;
        void* x;
}

void test42()
{
        size_t t;
        t = S42.y.offsetof;
        assert(t == 0);
        t = S42.x.offsetof;
        assert((t % (void*).sizeof) == 0);
}

/* ================================ */

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();
    test10();
    test11();
    test12();
    test13();
    test14();
    test15();
    test16();
    test17();
    test18();
    test19();
    test20();
    test21();
    test22();
    test23();
    test24();
    test25();
    test26();
    test27();
    test28();
    test29();
    test30();
    test31();
    test32();
    test35();
    test36();
    test37();
    test38();
    test39();
    test40();
    test41();
    test42();

    printf("Success\n");
    return 0;
}
