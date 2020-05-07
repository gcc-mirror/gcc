// RUNNABLE_PHOBOS_TEST
import std.stdio;
import core.stdc.stdio;

/*********************************************************/

template Foo(T)
{
    static if ( is(T : int) )
        alias T t1;

    static if (T.sizeof == 4)
        alias T t2;

    static if ( is(T AB : int) )
        alias AB t3;

    static if ( is(T* V : V*) )
        alias V t4;

    static if ( is(T W) )
        alias W t5;
    else
        alias char t5;

    static if ( is(T* X : X*) )
    {
    }
}

void test1()
{
    Foo!(int).t1 x1;
    assert(typeid(typeof(x1)) == typeid(int));

    Foo!(int).t2 x2;
    assert(typeid(typeof(x2)) == typeid(int));

    Foo!(int).t3 x3;
    assert(typeid(typeof(x3)) == typeid(int));

    Foo!(int).t4 x4;
    assert(typeid(typeof(x4)) == typeid(int));

    Foo!(int).t5 x5;
    assert(typeid(typeof(x5)) == typeid(int));

    Foo!(int).X x6;
    assert(typeid(typeof(x6)) == typeid(int));
}

/*********************************************************/


void test2()
{
    alias int T;

    static if ( is(T : int) )
        alias T t1;

    static if (T.sizeof == 4)
        alias T t2;

    static if ( is(T U : int) )
        alias U t3;

    static if ( is(T* V : V*) )
        alias V t4;

    static if ( is(T W) )
        alias W t5;
    else
        alias char t5;

    static if ( is(T* X : X*) )
    {
    }

    t1 x1;
    assert(typeid(typeof(x1)) == typeid(int));

    t2 x2;
    assert(typeid(typeof(x2)) == typeid(int));

    t3 x3;
    assert(typeid(typeof(x3)) == typeid(int));

    t4 x4;
    assert(typeid(typeof(x4)) == typeid(int));

    t5 x5;
    assert(typeid(typeof(x5)) == typeid(int));

    X x6;
    assert(typeid(typeof(x6)) == typeid(int));
}

/*********************************************************/

void test3()
{
    static if ( is(short : int) )
    {
        printf("1\n");
    }
    else
        assert(0);
    static if ( is(short == int) )
        assert(0);
    static if ( is(int == int) )
    {
        printf("3\n");
    }
    else
        assert(0);
}

/*********************************************************/

void test4()
{
    alias void Function(int);

    static if (is(Function Void == function))
        printf("if\n");
    else
        assert(0);

//    static if (is(Void == void))
//        printf("if\n");
//    else
//        assert(0);


    alias byte delegate(int) Delegate;

    static if (is(Delegate Foo == delegate))
        printf("if\n");
    else
        assert(0);

    static if (is(Foo Byte == function))
        printf("if\n");
    else
        assert(0);

//    static if (is(Byte == byte))
//        printf("if\n");
//    else
//        assert(0);


    union Union { }

    static if (is(Union == union))
        printf("if\n");
    else
        assert(0);


    struct Struct { }

    static if (is(Struct == struct))
        printf("if\n");
    else
        assert(0);


    enum Enum : short { EnumMember }

    static if (is(Enum Short == enum))
        printf("if\n");
    else
        assert(0);

    static if (is(Short == short))
        printf("if\n");
    else
        assert(0);

    class Class { }

    static if (is(Class == class))
        printf("if\n");
    else
        assert(0);


    interface Interface { }

    static if (is(Interface == interface))
        printf("if\n");
    else
        assert(0);

}

/*********************************************************/

class Foo5(T)
{
    Node sentinel;

    struct Node
    {
        int value;
    }
}

void test5()
{
    Foo5!(int) bar=new Foo5!(int);
    bar.sentinel.value = 7;
}


/*********************************************************/

template factorial6(int n)
{
    static if (n == 1)
        const int factorial6 = 1;
    else
        const int factorial6 = n * .factorial6!(n-1);
}

void test6()
{
    int i = factorial6!(4);
    printf("%d\n", i);
    assert(i == 24);
}


/*********************************************************/

template factorial7(float n, cdouble c, string sss, string ttt)
{
    static if (n == 1)
        const float factorial7 = 1;
    else
        const float factorial7 = n * 2;
}

template bar7(wstring abc, dstring def)
{
    const int x = 3;
}

void test7()
{
    float f = factorial7!(4.25, 6.8+3i, "hello", null);
    printf("%g\n", f);
    assert(f == 8.5);
    int i = bar7!("abc"w, "def"d).x;
    printf("%d\n", i);
    assert(i == 3);
}

/*********************************************************/

template whale(string walrus)
{
    const char [] whale = walrus;
}

template dolphin(string fish)
{
   const char [] dolphin = whale!(fish[0..3]);
}

const char [] urchin1 = dolphin!("anenome");
const char [] urchin2 = whale!("anenome"[0..3]);

template dolphin3(string fish)
{
   const char [] dolphin3 = fish[0..3];
}

const char [] urchin3 = dolphin3!("anenome");

template dolphin4(string fish)
{
  const char [] dolphin4 = whale!(fish[0..(3)]);
}

const char [] urchin4 = dolphin4!("anenome");

template dolphin5(string fish)
{
  const char [] dolphin5 = whale!(fish[(0)..3]);
}

const char [] urchin5 = dolphin5!("anenome");

void test8()
{
    assert(urchin1 == "ane");
    assert(urchin2 == "ane");
    assert(urchin3 == "ane");
    assert(urchin4 == "ane");
    assert(urchin5 == "ane");
}

/*********************************************************/

int testEmpty(string s) { return 0; }

template Recurse(string pattern){
}


template slice(string str, int from, int to)
{
    const string slice = str[from..to];
}


template Compile(string pattern)
{
    const string left = slice!(pattern,4,pattern.length);

    const string remaining = slice!(left,1,left.length);

    alias Recurse!(remaining) fn;
}



template Match(string pattern)
{
    alias Compile!(pattern) Match;
}

void test9()
{
    alias Match!("abcdefghijk") f;
}

/*********************************************************/

template Foo10(string s)
{
    const string Foo10 = s;
}

void test10()
{
    string s;

    s = Foo10!("abc" ~ "e");
    assert(s == "abce");

    s = Foo10!("abc" ~ 'f');
    assert(s == "abcf");

    s = Foo10!('g' ~ "abc");
    assert(s == "gabc");

    s = Foo10!('g' ~ "abc" ~ 'h');
    assert(s == "gabch");
}

/*********************************************************/

template Foo11(string s)
{
    const string Foo11 = s;
}

void test11()
{
    string s;

    s = Foo11!("abcdef"[1..$ - 1]);
    assert(s == "bcde");
}

/*********************************************************/

template Foo12(int i)
{
    const int Foo12 = i;
}

void test12()
{
    int i;

    i = Foo12!("abcdef" == "abcdef");
    assert(i == 1);
    i = Foo12!("abcdef" == "abcqef");
    assert(i == 0);
    i = Foo12!("abcdef" == "abc");
    assert(i == 0);
    i = Foo12!("abc" == "abcdef");
    assert(i == 0);
}

/*********************************************************/

const a13 = 3;
static if (a13 == 3)
    int b13 = 7;

template Foo13(int i)
{
    const int j = i + 1;
    static if (j == 3)
        const int k = 2;
}

void test13()
{
    assert(b13 == 7);
    assert(Foo13!(2).k == 2);
}

/*********************************************************/

template zebra(string w)
{
    static if (w.length > 2 && w[1] == 'q')
        const bool zebra = true;
    else
        const bool zebra = false;
}

template horse(string w)
{
    static if (w.length == 1 || w[1] == 'q')
        const bool horse = true;
    else
        const bool horse = false;
}

void test14()
{
    bool lion = zebra!("a");
    writeln(lion);
    assert(!lion);
    lion = zebra!("aqb");
    writeln(lion);
    assert(lion);

    lion = horse!("a");
    writeln(lion);
    assert(lion);
    lion = horse!("aqb");
    writeln(lion);
    assert(lion);
    lion = horse!("ab");
    writeln(lion);
    assert(!lion);
}

/*********************************************************/

template factorial15(int n)
{
   static if (n<2) const int factorial15 = 1;
   else const int factorial15 = n * factorial15!(n-1);
}

template rhino15(alias hippo)
{
   const int rhino15 = hippo!(3);
}

void test15()
{
    const int lion = rhino15!(factorial15);
    assert(lion == 6);
}

/*********************************************************/

// Create a constant array of int or uint sized items
// as a dstring string. n is the index of the last item.
template makeLookup(alias entry, int n)
{
   static if (n == -1) // start with an empty array...
     const dchar [] makeLookup = "";
   else        // ... and fill it up
     const dchar [] makeLookup = makeLookup!(entry, n-1)
           ~ cast(dchar)entry!(n);
}

template factorial16(uint n)
{
     static if (n<2) const uint factorial16 = 1;
     else const factorial16 = n * factorial16!(n-1);
}

// Make an array of factorials from 0 to 13 (14!> uint.max)
const smallfactorials = makeLookup!(factorial16, 13);

const uint[14] testtable =
[
  1,
  1,
  2,
  6,
  24,
  120,
  720,
  5040,
  40320,
  362880,
  3628800,
  39916800,
  479001600,
  1932053504,
];

void test16()
{
    for (int i=0; i<smallfactorials.length; ++i)
    {
        writefln("%d  %d", i, smallfactorials[i]);
        assert(smallfactorials[i] == testtable[i]);
    }
}

/*********************************************************/

template dingo(int a)
{
    pragma(msg, "This actually gets evaluated!");
    static if (a==2) {
        const int dingo = 28;
    }
    else
        const int dingo = a;
}

const int bilby = dingo!(2);

void test17()
{
    assert(bilby == 28);
}

/*********************************************************/

template frog(char F)
{
     const int frog = 1;
}

template frog(int F)
{
     const int frog = 2;
}

template frog(char F: 'A')
{
     const int frog = 3;
}

template frog(int F: 65)
{
     const int frog = 4;
}

static assert( frog!('B')==1);
static assert( frog!(64)==2);
static assert( frog!('A')==3);
static assert( frog!(65)==4);

void test18()
{
}

/*********************************************************/

void test19()
{
    MakeTuple19!(int,int) t;
    assert(t.sizeof == int.sizeof);
}

template MakeTuple19( T1, T2 )
{
     alias int MakeTuple19;
}

template MakeTuple19(T1, T2, T3=float)
{
     alias long MakeTuple19;
}


/*********************************************************/

template sqrt(real x, real root = x/2, int ntries = 0)
{
  static if (ntries == 5)
    // precision doubles with each iteration,
    // 5 should be enough
    const sqrt = root;
  else static if (root * root - x == 0)
    const sqrt = root;  // exact match
  else
    // iterate again
    const sqrt = sqrt!(x, (root+x/root)/2, ntries+1);
}

void test20()
{
    real x = sqrt!(2);
    writefln("%.20g", x); // 1.4142135623730950487
}

/*********************************************************/

template hash(string s, uint sofar=0)
{
   static if (s.length == 0)
      const hash = sofar;
   else
      const hash = hash!(s[1 .. $], sofar * 11 + s[0]);
}

uint foo21()
{
    return hash!("hello world");
}

void test21()
{
    auto i = foo21();
    writeln(i);
    assert(i == 1871483972);
}


/*********************************************************/

T Foo22(T)(int i)
{
    printf("i = %d\n", i);
    return cast(T)0;
}

void test22()
{
    Foo22!(int)(3);
}

/*********************************************************/

template fish( char s)
{
  const bool fish = true;
}

template dog(string bird)
{
        static if (bird.length>99 && fish!( (bird[95])) )
           const int dog = 2;
        else const int dog = 3;
}

const int pig = dog!("a");

void test23()
{
}

/*********************************************************/

T delegate (T) acc24 (T) (T n)
{
    return (T i) { return n += i; };
}

void test24()
{
    auto acc1 = acc24 (4);
}

/*********************************************************/

T func25(T, T c = 1)(T x)
{
    return x * c;
}

void test25()
{
    double d;

    d = func25(1.0);
    assert(d == 1.0);

    d = func25(2.0);
    assert(d == 2.0);

    d = func25!(double)(2.0);
    assert(d == 2.0);

    d = func25!(double, 3)(2.0);
    assert(d == 6.0);
}

/*********************************************************/

class Foo26 {}
class Bar26 {}

string name26;

template aliastest(alias A) {
    pragma(msg,"Alias Test instantiated");
    void aliastest() {
        name26 = (new A!().al).classinfo.name;
        //writefln("Alias Test: ", name26);
    }
}

template boxtpl(alias A) {
    template box() {
        alias A al;
    }
}

void test26()
{
    aliastest!(boxtpl!(Foo26).box) ();
    assert(name26 == "template4.Foo26");
    aliastest!(boxtpl!(Bar26).box) ();
    assert(name26 == "template4.Bar26");
}

/*********************************************************/

struct TFoo27(int x) { }
alias TFoo27!(3) a;
alias TFoo27!(2+1) b;
alias TFoo27!(3u) c;

static assert(is(TFoo27!(3) == TFoo27!(2 + 1)));
static assert(is(TFoo27!(3) == TFoo27!(3u)));

void test27()
{
}

/*********************************************************/

struct SiQuantity
{
    real value = 0;
    static assert(SiQuantity.sizeof == real.sizeof);

    template AddDimensions(int mul, U) { }
}

void test28()
{
}

/*********************************************************/

template Count29() { const Count29 = 5; }

void test29()
{
    int[Count29!()] x;

    assert(x.length == 5);
}

/*********************************************************/

class FooClass(T) {  T data; }

struct FooStruct(T) {  T data;  }

void bar_struct(T)(FooStruct!(T) a) {}

void bar_class(T)(FooClass!(T) a) {}

void test30()
{
    auto C = new FooClass!(double);
    FooStruct!(double) S;

    bar_struct(S);
    bar_class!(double)(C);
    bar_class(C);
}

/*********************************************************/

V get31(V,K)(V[K] dict, K key, V def = V.init)
{
    V* ptr = key in dict;
    return ptr? *ptr: def;
}


string get31x(string[int] dict, int key, string def = null)
{
    string* ptr = key in dict;
    return ptr? *ptr: def;
}


void test31()
{
    string[int] i2s;
    i2s[1] = "Hello";
    i2s[5] = "There";

    writeln( i2s.get31(1, "yeh") );
    writeln( i2s.get31(2, "default") );
    writeln( i2s.get31(1) );
    writeln( i2s.get31(2) );
}

/*********************************************************/

void delegate(T, S) Concat(S, T...)(void delegate(T) one, void delegate(S) two)
{
  return( delegate void(T t, S s){} );
}

void test32()
{
  void delegate(char, char, int) wtf = Concat(
    delegate void(char a, char b) {},
    delegate void(int lol) {}
  );
}

/*********************************************************/

struct Composer(T) {
    alias T delegate(T) Fun;
    Fun[] funs;
    public T opCall()(T x) {
        T result = x;
        foreach_reverse (f; funs)
        {
            result = f(result);
        }
        return result;
    }
    public void opAddAssign(Fun f) {
        funs ~= f;
    }
}

struct square(T) {
    T opCall(T t) {
        return t*t;
    }
}

struct plus1(T) {
    T opCall(T t) {
        return t+1;
    }
}

struct div3(T) {
    T opCall(T t) {
        return t/3.0;
    }
}

T delegate(T) tofp(T : S!(T), alias S)()
{
    class Foo
    {
        div3!(T) arg;

        T bar(T t)
        {
            return arg(t);
        }
    }

    Foo f = new Foo;
    return &f.bar;
}

void test33() {
    Composer!(double) comp;
    comp += delegate double (double x) { return x/3.0;};
    comp += delegate double (double x) { return x*x;};
    comp += (double x) => x + 1.0;
    writefln("%f", comp(2.0));

    // Try function objects
    Composer!(double) comp2;
    comp2 += tofp!(div3!(double))();
    comp2 += tofp!(square!(double))();
    comp2 += tofp!(plus1!(double))();
    writefln("%f", comp2( 2.0));
}

/*********************************************************/

template Print34(Ts ...) { pragma (msg, Ts.stringof); }

template Tuple34(Ts ...) { alias Ts Tuple34; }

template Decode34( T )                                       { alias Tuple34!() Types; }
template Decode34( T : TT!(U1),       alias TT, U1 )         { alias Tuple34!(U1) Types; }
template Decode34( T : TT!(U1,U2),    alias TT, U1, U2 )     { alias Tuple34!(U1,U2) Types; }
template Decode34( T : TT!(U1,U2,U3), alias TT, U1, U2, U3 ) { alias Tuple34!(U1,U2,U3) Types; }


struct S34_1(T1) {}
struct S34_2(T1, T2) {}
struct S34_3(T1, T2, T3) {}

alias Decode34!( bool ).Types SQ0;
alias Decode34!( S34_1!(bool) ).Types SQ1;
alias Decode34!( S34_2!(bool,short) ).Types SQ2;
alias Decode34!( S34_3!(bool,short,int) ).Types SQ3;

mixin Print34!(SQ0);
mixin Print34!(SQ1);
mixin Print34!(SQ2);
mixin Print34!(SQ3);

void test34()
{
}

/*********************************************************/

template strof(T)
{
    static string strof = T.stringof;
}

void test35()
{
    alias typeof(delegate () { return (char[]).init;} ) del;
    auto a = strof!(del);
    auto aa = strof!(int);
    auto ab = strof!(typeof(5));
    auto meth = delegate () { return (char[]).init;};
    auto b = strof!(typeof(meth));
    auto c = strof!(typeof(delegate () { return 5; } ));
    auto d = strof!(typeof(delegate () { return (char[]).init;} ));
}

/*********************************************************/

struct Number36(int N)
{
    const int value = N;
}

struct Foo36(T)
{
    int talk() { printf("Not so special:\n"); return 1; }
}

struct Foo36(T : Number36!(N), int N)
{
    int talk() { printf("Ooh special - NUMBER N\n"); return 2; }
}

void test36()
{
    Foo36!(Number36!(5)) x;
    auto i = x.talk();
    assert(i == 2);
}

/*********************************************************/

struct Base37(T) {}

alias Base37 Alias37;

void foo37(T)(Alias37!(T) x) {}

void test37()
{
   Base37!(float) b;
   foo37(b); // fails!
}

/*********************************************************/

void sort(alias dg, T)(T[] arr)
{
    bool a = dg(1,2);
    printf("a = %d\n", a);
    assert(a == true);
}

void test38()
{   int[] arr;
    int i = 3;

    sort!( (x,y){ return x + i > y; } )(arr);
    sort!( (int x,int y){ return x + i > y; } )(arr);
}

/*********************************************************/

void bug4652(U, T...)(long y, T x, U num) {}
void bug4652default(T) (T value, int x=2) {}
void bug4652default(T) (T value, int y) {}
void bug4676(T...)(T args, string str) {}
void bug4676(T...)(T args) {}

void instantiate4652()
{
    bug4652(2, 'c', 27, 'e', 'f',1); // rejects-valid
    bug4652(2, 1);  // infinite loop on valid code
    bug4652default(true);
    bug4676(1, 2, 3);
}

/*********************************************************/
// 7589

struct T7589(T)
{
    void n;
}
static assert(!__traits(compiles, T7589!(int)));

int bug7589b(T)() @safe { int *p; *(p + 8) = 6; }
static assert(!__traits(compiles, bug7589b!(int)()+7 ));


/*********************************************************/

int bar39(alias dg)(int i)
{
    return dg(i);
}

void test39()
{
    auto i = bar39!(a => a + 1)(3);
    if (i != 4)
        assert(0);
}

/*********************************************************/
// 6701

uint foo_6701(uint v:0)() { return 1; }
uint foo_6701(uint v)() { return 0; }
uint foo2_6701(uint v:0, string op)() { return 1; }
uint foo2_6701(uint v, string op)() { return 0; }

void test6701()
{
    assert(foo_6701!(0u)() == 1);
    assert(foo2_6701!(0u, "+")() == 1);
}

/******************************************/
// 7469

struct Foo7469a(int x) { }
struct Foo7469b(int x) { }
struct Foo7469c(alias v) { }
struct Foo7469d(T...) { }
struct Foo7469e(int a, T...) { }
struct Foo7469f(T, int k=1) { }
struct Foo7469g(T, int k=1) { }

void test7469()
{
    static assert(Foo7469a!(3 )    .mangleof[$-28 .. $] == "17__T8Foo7469aVii3Z8Foo7469a");
    static assert(Foo7469a!(3u)    .mangleof[$-28 .. $] == "17__T8Foo7469aVii3Z8Foo7469a");
    static assert(Foo7469b!(3u)    .mangleof[$-28 .. $] == "17__T8Foo7469bVii3Z8Foo7469b");
    static assert(Foo7469b!(3 )    .mangleof[$-28 .. $] == "17__T8Foo7469bVii3Z8Foo7469b");
    static assert(Foo7469c!(3 )    .mangleof[$-28 .. $] == "17__T8Foo7469cVii3Z8Foo7469c");
    static assert(Foo7469c!(3u)    .mangleof[$-28 .. $] == "17__T8Foo7469cVki3Z8Foo7469c");
    static assert(Foo7469d!(3 )    .mangleof[$-28 .. $] == "17__T8Foo7469dVii3Z8Foo7469d");
    static assert(Foo7469d!(3u)    .mangleof[$-28 .. $] == "17__T8Foo7469dVki3Z8Foo7469d");
    static assert(Foo7469e!(3u, 5u).mangleof[$-32 .. $] == "21__T8Foo7469eVii3Vki5Z8Foo7469e");
    static assert(Foo7469f!(int, 1).mangleof[$-30 .. $] == "19__T8Foo7469fTiVii1Z8Foo7469f");
    static assert(Foo7469f!(int)   .mangleof[$-30 .. $] == "19__T8Foo7469fTiVii1Z8Foo7469f");
    static assert(Foo7469g!(int)   .mangleof[$-30 .. $] == "19__T8Foo7469gTiVii1Z8Foo7469g");
    static assert(Foo7469g!(int, 1).mangleof[$-30 .. $] == "19__T8Foo7469gTiVii1Z8Foo7469g");
}

/******************************************/

template foo7698a(T, T val : 0)
{
    enum foo7698a = val;
}

T foo7698b(T, T val : 0)()
{
    return val;
}

T foo7698c(T, T val : T.init)()
{
    return val;
}

void test7698()
{
    static assert(foo7698a!(int, 0) == 0);
    assert(foo7698b!(int, 0)() == 0);
    assert(foo7698c!(int, 0)() == 0);
}

/*********************************************************/

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
    test33();
    test34();
    test35();
    test36();
    test37();
    test38();
    test39();
    test6701();
    test7698();

    printf("Success\n");
    return 0;
}
