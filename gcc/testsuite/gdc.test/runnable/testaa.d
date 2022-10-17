// PERMUTE_ARGS: -fPIC

/* Test associative arrays */

extern(C) int printf(const char*, ...);
extern(C) int memcmp(const void *s1, const void *s2, size_t n);

import core.memory;  // for GC.collect

/************************************************/

int[char[]] nametable;

void insert(string name, int value)
{
    nametable[name] = value;
}

int retrieve(string name)
{
    return nametable[name];
}

void test1()
{   int v;

    printf("test1.a\n");
    insert("hello", 1);
    printf("test1.b\n");
    insert("world", 2);
    printf("test1.c\n");
    v = retrieve("hello");
    assert(v == 1);
    v = retrieve("world");
    assert(v == 2);
    v = retrieve("world");
    assert(v == 2);

    nametable.rehash;
    v = retrieve("world");
    assert(v == 2);
}

/************************************************/


void test2()
{
    int[string] aa;
    string[] keys;
    int[] values;

    printf("test2()\n");

    /*************/

    assert(aa == null);
    assert(aa.length == 0);

    keys = aa.keys;
    assert(keys.length == 0);

    values = aa.values;
    assert(values.length == 0);

    aa.rehash;
    assert(aa.length == 0);

    /*************/

    aa["hello"] = 3;
    assert(aa["hello"] == 3);
    aa["hello"]++;
    assert(aa["hello"] == 4);

    assert(aa.length == 1);

    keys = aa.keys;
    assert(keys.length == 1);
    assert(memcmp(keys[0].ptr, cast(char*)"hello", 5) == 0);

    values = aa.values;
    assert(values.length == 1);
    assert(values[0] == 4);

    aa.rehash;
    assert(aa.length == 1);
    assert(aa["hello"] == 4);
}

/************************************************/

void test4()
{
    int[const(ubyte)[]] b;
    const(ubyte)[] x;
    b[x] = 3;
    assert(b[x] == 3);
}

/************************************************/

void test5()
{
    int[immutable(short)[]] b;
    immutable(short)[] x;
    b[x] = 3;
    assert(b[x] == 3);
}

/************************************************/

void test6()
{
    int[const(int)[]] b;
    const(int)[] x;
    b[x] = 3;
    assert(b[x] == 3);
}

/************************************************/

void test7()
{
    int[immutable(uint)[]] b;
    immutable(uint)[] x;
    b[x] = 3;
    assert(b[x] == 3);
}

/************************************************/

void test8()
{
    int[immutable(long)[]] b;
    immutable(long)[] x;
    b[x] = 3;
    assert(b[x] == 3);
}

/************************************************/

void test9()
{
    int[immutable(ulong)[]] b;
    immutable(ulong)[] x;
    b[x] = 3;
    assert(b[x] == 3);
}

/************************************************/

class A10 {}

int[immutable(A10)[]] foo10;

void test10()
{
    auto key = new immutable(A10)[2];

    cast()(key[0]) = new A10();
    foo10[key] = 0;
    assert(key in foo10);
    assert(!(key !in foo10));
}


/************************************************/

struct Value
{
    uint x,y,z,t;
}

struct Key
{
    int a,b,c,d;

    static int hash, cmp, equals;

    size_t toHash() const
    {
        hash = 1;
        return a + b + c + d;
    }

    int opCmp(ref const Key s) const
    {
        cmp = 1;
        int x;

        x = a - s.a;
        if (x == 0)
        {   x = b - s.b;
            if (x == 0)
            {   x = c - s.c;
                if (x == 0)
                    x = d - s.d;
            }
        }
        return x;
    }

    bool opEquals(ref const Key s) const
    {
        printf("opEquals()\n");
        equals = 1;
        return (a == s.a && b == s.b && c == s.c && d == s.d);
    }
}

void test11()
{
    Value[Key] table;

    Value* p;
    Value v;
    Value r;
    Key k;

    v.x = 7;
    v.y = 8;
    v.z = 9;
    v.t = 10;

    k.a = 1;
    k.b = 2;
    k.c = 3;
    k.d = 4;

    p = k in table;
    assert(!p);

    table[k] = v;
    p = k in table;
    assert(p);

    table.rehash;
    p = k in table;
    assert(p);

    r = table[k];
    assert(v == r);

    table.remove(k);
    assert(!(k in table));

    printf("Key.hash = %d\n", Key.hash);
    assert(Key.hash == 1);
    printf("Key.cmp = %d\n", Key.cmp);
    printf("Key.equals = %d\n", Key.equals);
    assert(Key.cmp == 1 && !Key.equals || !Key.cmp && Key.equals == 1);
}


/************************************************/

struct S12
{
    byte number;
    char[] description;
    char[] font_face;
    byte font_size;
    ushort flags;
    int colour_back;
    int colour_fore;
    byte charset;
}

void test12()
{
    S12[] x;
    printf("size %zd\n",S12.sizeof);
    printf("align %zd\n",S12.alignof);
    printf("offset %zd\n",S12.description.offsetof);

    for (int i=0;i<3;i++) {
        S12 s;
        s.font_face="font face".dup;
        x ~= s;
    }

/* works fine
    S12 s;
    s.font_face="font face".dup;
    x ~= s;
    s.font_face="font face".dup;
    x ~= s;
    s.font_face="font face".dup;
    x ~= s;
    s.font_face="font face".dup;
    x ~= s;
*/
    GC.collect();
    printf("%.*s\n", cast(int)x[0].font_face.length, x[0].font_face.ptr);
    printf("%.*s\n", cast(int)x[1].font_face.length, x[1].font_face.ptr);
}


/************************************************/

void test13()
{
    int[string] array;
    array["eins"]=1;
    array["zwei"]=2;
    array["drei"]=3;

    assert(array.length==3);

    int[string] rehashed=array.rehash;
    assert(rehashed is array);

    string[] key = array.keys;
    assert(key.length==3);

    bool[3] have;

    assert(!have[0]);
    assert(!have[1]);
    assert(!have[2]);

    foreach(string value; key){
        switch(value){
            case "eins":{
                have[0]=true;
                break;
            }case "zwei":{
                have[1]=true;
                break;
            }case "drei":{
                have[2]=true;
                break;
            }default:{
                assert(0);
            }
        }
    }

    assert(have[0]);
    assert(have[1]);
    assert(have[2]);
}

/************************************************/

void test14()
{
    int[char[]] aa;

    aa["hello"] = 3;
    assert(aa["hello"] == 3);
    assert("hello" in aa);
    //delete aa["hello"];
    aa.remove("hello");
    assert(!("hello" in aa));
}

/************************************************/

class SomeClass
{
    this(char value)
    {
        printf("class created\n");
        _value = value;
    }

    ~this()
    {
        printf("class killed (%d)\n", _value);
    }

    char value()
    {
        return _value;
    }

    private
    {
        char _value;
    }
}

char[] allChars = [ 'a', 'b', 'c', 'e', 'z', 'q', 'x' ];

SomeClass[char] _chars;

void _realLoad()
{
    printf("Loading...\n");
    foreach(char ch; allChars)
    {
        _chars[ch] = new SomeClass(ch);
    }
}



void test15()
{
    _realLoad();
    int j;

    for (int i = 0; i < 10000; i++)
    {
        foreach(char ch; allChars)
        {
            SomeClass obj = _chars[ch];
            j += obj.value;
        }
        GC.collect();
    }
    printf("j = %d\n", j);
    assert(j == 7500000);
}


/************************************************/

void test16()
{
    int[int] aa;

    for (int i = 0; i < 50000; i++)
    {
        aa[i] = i;
    }

    int[] keys = aa.keys;
    assert(keys.length == aa.length);

    int j;
    foreach (k; keys)
    {
        assert(k in aa);
        j += aa[k];
    }
    assert(j == 1249975000);

    int m;
    foreach (k, v; aa)
    {
        assert(k in aa);
        assert(aa[k] == v);
        m += v;
    }
    assert(j == m);

    m = 0;
    foreach (v; aa)
    {
        m += v;
    }
    assert(j == m);

    int[] values = aa.values;
    assert(values.length == aa.length);

    foreach(k; keys)
    {
        aa.remove(k);
    }
    assert(aa.length == 0);

    for (int i = 0; i < 1000; i++)
    {
        aa[i] = i;
    }
    foreach(k; aa)
    {
        if (k < 1000)
            break;
    }
    foreach(k, v; aa)
    {
        if (k < 1000)
            break;
    }
}

/************************************************/

void dummy17()
{
}

int[string] bb17;

int foo17()
{
    foreach(string s, int i; bb17)
    {
        dummy17();
    }

    bb17["a"] = 1;

    foreach(int b; bb17)
    {
        try{
            throw new Error("foo");
        }catch(Error e){
            assert(e);
            return 0;
        }catch(Throwable){
            assert(0);
        }
        assert(0);
    }

    assert(0);
}

void test17()
{
    int i = foo17();
    printf("foo17 = %d\n", i);
    assert(i == 0);
}

/************************************************/

void test18()
{
    int[uint] aa;

    aa[1236448822] = 0;
    aa[2716102924] = 1;
    aa[ 315901071] = 2;

    aa.remove(1236448822);
    printf("%d\n", aa[2716102924]);
    assert(aa[2716102924] == 1);
}


/************************************************/

void test19()
{
    immutable(char[5])[int] aa = ([3:"hello", 4:"betty"]);

    assert(aa[3] == "hello");
    assert(aa[4] == "betty");

    auto keys = aa.keys;
    printf("%d\n", keys[0]);
    printf("%d\n", keys[1]);

    auto vs = aa.values;
    printf("%.*s\n", cast(int)vs[0].length, vs[0].ptr);
    printf("%.*s\n", cast(int)vs[1].length, vs[1].ptr);

    string aavalue_typeid = typeid(typeof(aa.values)).toString();
    printf("%.*s\n", cast(int)aavalue_typeid.length, aavalue_typeid.ptr);

    printf("%.*s\n", cast(int)aa[3].length, aa[3].ptr);
    printf("%.*s\n", cast(int)aa[4].length, aa[4].ptr);
}

/************************************************/

void test20()
{
    string[int] aa = ([3:"hello", 4:"betty"]);

    assert(aa[3] == "hello");
    assert(aa[4] == "betty");

    auto keys = aa.keys;
    printf("%d\n", keys[0]);
    printf("%d\n", keys[1]);

    auto values = aa.values;
    printf("%.*s\n", cast(int)values[0].length, values[0].ptr);
    printf("%.*s\n", cast(int)values[1].length, values[1].ptr);

    string aavalue_typeid = typeid(typeof(aa.values)).toString();
    printf("%.*s\n", cast(int)aavalue_typeid.length, aavalue_typeid.ptr);

    printf("%.*s\n", cast(int)aa[3].length, aa[3].ptr);
    printf("%.*s\n", cast(int)aa[4].length, aa[4].ptr);
}

/************************************************/

void test21()
{
    ushort[20] key = 23;
    int[ushort[20]] aa;
    aa[key] = 42;
    auto x = aa[key];
    assert(x == 42);
    printf("foo\n");
}

/************************************************/

void test22()
{
    int[string] stopWords = [ "abc"[]:1 ];
    assert("abc"[] in stopWords);
}

/************************************************/

void test23()
{
    uint[char[]][] fractal;
    fractal.length = 10;
}

/************************************************/

void test24()
{
    int[string] x;
    char[] y;
    if (y in x)
    {
        int z = x[y];
    }
}

/************************************************/

void test25()
{
    string[string] aa;
    foreach (k,v; aa)
    {
    }
}

/************************************************/

class Tag
{
    string[string] attr;
}

void foo26(const(Tag) tag_)
{
    foreach(k,v;tag_.attr) { }
}

void test26()
{
}

/************************************************/

void test27()
{
    int[int] s;
    s = s.init;
}

/************************************************/

void test28()
{
    auto a1 = [ 1:10.0, 2:20, 3:15 ];
    auto a2 = [ 1:10.0, 2:20, 3:15 ];
    assert(a1 !is a2);
    assert(a1 == a2);
    a2[7] = 23;
    assert(a1 != a2);
    a2.remove(7);
    assert(a1 == a2);
    a1.rehash;
    assert(a1 == a2);
    a2[2] = 18;
    assert(a1 != a2);
}

/************************************************/

void test29()
{
    auto gammaFunc = [-1.5:2.363, -0.5:-3.545, 0.5:1.772];

    // write all keys
    foreach (k; gammaFunc.byKey()) {
       printf("%f\n", k);
    }

    // write all values
    foreach (v; gammaFunc.byValue()) {
       printf("%f\n", v);
    }
}

/************************************************/

string toString(int value)
{
    char[] result = new char[12];

    uint ndigits = 0;
    do
    {
        const c = cast(char) ((value % 10) + '0');
        value /= 10;
        ndigits++;
        result[$ - ndigits] = c;
    }
    while (value);
    return cast(string) result[$ - ndigits .. $];
}

void test30()
{
    int[string] aa;
    for(int i = 0; i < 100000; i++)
    {
        string s = toString(i);
        aa[s] = i;
    }
}

/************************************************/

void test31()
{
    int[int] test;
    test[0] = 0;
    test[1] = 1;
    test[2] = 2;

    bool flag = false;
    foreach( k, v; test){
        //printf("loop: %d %d\n", k, v);
        assert(!flag);
        flag = true;
        break;
    }
}

/************************************************/

void test32()
{
    uint[ushort] aa;
    aa[1] = 1;
    aa[2] = 2;
    aa[3] = 3;
    aa[4] = 4;
    aa[5] = 5;
    foreach(v; aa)
    {
        printf("%x\n", v);
        assert(v >= 1 && v <= 5);
    }
}

/************************************************/

template ICE3996(T : V[K], K, V) {}

struct Bug3996 {}

static assert(!is( ICE3996!(Bug3996) ));

/************************************************/

void bug4826c(T)(int[int] value, T x) {}

void test4826c()
{
    AssociativeArray!(int, int) z;
    bug4826c(z,1);
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=5131

struct ICE5131
{
    this(int n) {}
    ICE5131 opAssign(int x) { return this; }
}

void test5131()
{
    ICE5131[string] a;
    a["ICE?"] = 1;  // call ctor
    a["ICE?"] = 1;  // call opAssign
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6178

bool test6178a()
{
    // AA value setting through identity opAssign

    int assign = 0;
    struct S
    {
        int value = 10;

        void opAssign(S rhs)
        {
            ++assign;
            assert(value == 10);
        }
    }

    int count = 0;
    int makeKey() { return ++count; }

    S[int] aa;
    assert(aa.length == 0);

    aa[makeKey()] = S();
    assert(assign == 0);
    assert(aa.length == 1 && 1 in aa);

    aa[1] = S();
    assert(assign == 1);
    assert(aa.length == 1 && 1 in aa);

    return true;
}

bool test6178b()
{
    // AA value setting through implicit ctor call + non-identity opAssign

    int ctor = 0;
    int assign = 0;
    struct S
    {
        int value = 10;

        @disable this();

        this(int n)
        {
            ++ctor;
            assert(value == 10);
            value = 20;
        }
        void opAssign(int rhs)
        {
            ++assign;
            assert(value == 20);
            assert(rhs == 30);
            value = rhs;
        }
    }

    int count = 0;
    int makeKey() { return ++count; }

    S[int] aa;
    assert(aa.length == 0);

    aa[makeKey()] = 20;
    assert(assign == 0 && ctor == 1 && count == 1);
    assert(aa.length == 1 && (1 in aa));

    aa[1] = 30;
    assert(assign == 1 && ctor == 1);
    assert(aa.length == 1 && 1 in aa);

    return true;
}

bool test6178c()
{
    // AA value setting through non-identity opAssign

    struct S
    {
        //this(int) {}
        // not possible to perform implicit ctor call
        void opAssign(int) {}
    }

    S[int] aa;
    assert(aa.length == 0);

    if (!__ctfe)
    {
        // currently CTFE does not support throwing RangeError
        import core.exception : RangeError;
        try { aa[1] = 1; assert(0); } catch (RangeError) {}

        // The above line is exactly same as:
        try { aa[1].opAssign(1); assert(0); } catch (RangeError) {}
    }
    assert(aa.length == 0);

    aa[1] = S();
    aa[1] = 1;
    assert(aa.length == 1);

    return true;
}

bool test6178d()
{
    // AA value setting through implicit ctor call + alias this

    int ctor;
    struct S
    {
        this(int n) { ++ctor; value = n; }

        int value;
        alias value this;
    }

    S[int] aa;
    assert(ctor == 0);
    assert(aa.length == 0);

    aa[1] = 0;      // implicit ctor call + blit assign
    assert(aa[1].value == 0 && ctor == 1);
    assert(aa.length == 1);

    aa[1] = 1;      // set through alias this
    assert(aa[1].value == 1 && ctor == 1);
    assert(aa.length == 1);

    return true;
}

bool test6178e()
{
    // AA value setting through alias this

    struct S
    {
        int value;
        alias value this;
    }

    S[int] aa;
    assert(aa.length == 0);

    if (!__ctfe)
    {
        // currently CTFE does not support throwing RangeError
        import core.exception : RangeError;
        try { aa[1] = 1; assert(0); } catch (RangeError) {}

        // The above line is exactly same as:
        try { aa[1].value = 1; assert(0); } catch (RangeError) {}
    }
    assert(aa.length == 0);

    aa[1] = S(0);   // construct + blit assign
    assert(aa[1].value == 0 && aa.length == 1);

    aa[1] = 1;      // set through alias this
    assert(aa[1].value == 1 && aa.length == 1);

    return true;
}

void test6178()
{
    static assert(test6178a()); // ctfe check
    test6178a();                // runtime test

    static assert(test6178b());
    test6178b();

    static assert(test6178c());
    test6178c();

    static assert(test6178d());
    test6178d();

    static assert(test6178e());
    test6178e();
}

void test6178x()
{
    return; // depends on AA implementation
    static int ctor, cpctor, dtor;

    static struct S
    {
        this(int)  { ++ctor;   printf("ctor\n");   }
        this(this) { ++cpctor; printf("cpctor\n"); }
        ~this()    { ++dtor;   printf("dtor\n");   }
    }
    static struct X
    {
        this(int) {}
        void opAssign(int) {}
    }

    X[S] aa1;
    S[int] aa2;

    {
        auto value = S(1);
        assert(ctor==1 && cpctor==0 && dtor==0);

        ref getRef(ref S s = value) { return s; }
        auto getVal() { return value; }

        aa1[value] = 10;
        assert(ctor==1 && cpctor==1 && dtor==0); //call copy ctor when we putting 'value' to aa1

        aa1[getRef()] = 20;
        assert(ctor==1 && cpctor==1 && dtor==0); //copy ctor wasn't called because we didn't create a new entry in aa, using an existing key

        aa1[getVal()] = 20;
        assert(ctor==1 && cpctor==2 && dtor==1); //call copy ctor and dtor, because we pass key by value

        aa2[1] = value;
        assert(ctor==1 && cpctor==3 && dtor==1); //call copy ctor when we putting `value` to aa2[1]

        aa2[2] = getRef();
        assert(ctor==1 && cpctor==4 && dtor==1); //call copy ctor when we putting `value` to aa2[2]
    }
    assert(ctor==1 && cpctor==4 && dtor==2);     //We've got 3 "S" instances that aren't destroyed yet: the key in aa1, aa2[1], aa2[2].
    assert(ctor + cpctor - aa2.length - aa1.length == dtor);
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10595

struct S10595
{
    bool b = true;

    bool test()
    {
        if (!b)  // note: must be a check, not 'return b;'
            return false;

        return true;
    }
}

struct Wrap10595
{
    int i;
    alias i this;
    S10595 s;
}

void test10595()
{
    {
        Wrap10595[int] wrap;

        wrap[0] = Wrap10595();
        wrap[0].i = 0;

        assert(wrap[0].s.test());  // ok
    }

    {
        Wrap10595[int] wrap;

        wrap[0] = Wrap10595();

        assert(wrap[0].s.test());  // failure
    }
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10970

struct RefCounted10970(T) //if (!is(T == class))
{
    struct RefCountedStore
    {
    }
    RefCountedStore _refCounted;

    this(this) {}

    ~this() {}
}

struct Array10970(T) if (!is(T : const(bool)))
{
    struct Payload
    {
    }
    RefCounted10970!Payload _data;
}

class C10970
{
    this(string name)
    {
        m[name] = Arr();
    }

    alias Array10970!C10970 Arr;
    Arr[string] m;
}

void test10970()
{
    C10970 c = new C10970("test");
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6433

void test6433()
{
    int[int] aa;
    static assert(aa.sizeof != 0);
    static assert(aa.alignof != 0);
    static assert(is(typeof(aa.init) == int[int]));
    static assert(typeof(aa).mangleof == "Hii");
    static assert(typeof(aa).stringof == "int[int]");
    static struct AA { int[int] aa; }
    static assert(AA.aa.offsetof == 0);

    aa = aa.init;
    aa[0] = 1;
    assert(aa.length == 1 && aa[0] == 1);
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6612

void test6612()
{
    auto aa1 = [1: 2]; // OK
    auto aa2 = [4: 5]; // OK
    int[int[int]] aa3 = [aa1:3, aa2:6]; // OK
    int[int[int]] aa4 = [[1:2]:3, [4:5]:6]; // error
    int[int[string]] aa5 = [["a":1]:2, ["b":3]:4];
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=7365

struct TickDuration
{
    bool opEquals(ref const TickDuration rhs) const
    {
        return true;
    }
}

void test7365()
{
    TickDuration[Object] aa;
    aa.keys;
}

/************************************************/

enum aa5520 = [5 : "hello"];

void test5520()
{
    auto a = aa5520.values;
}

/************************************************/

enum size_t N6655 = 1;
int[bar6655.length] foo6655;
int[N6655] bar6655;

/************************************************/

struct ChunkLoc {}

ChunkLoc Get()
{
    return ChunkLoc();
}

void test6799()
{
    int[ChunkLoc] aa;
    aa.remove(Get());
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11359

void test11359()
{
    class Bar {}
    static Bar[string] aa;
    static ref fun() { return aa; }

    string key = "test";

    fun[key] = new Bar;
    assert(aa.length == 1);
    Bar bar = fun[key];
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11730

struct SysTime11730
{
    ref SysTime11730 opAssign(SysTime11730 rhs)
    {
        assert(0);
    }
}

struct Nullable11730(T)
{
    T _value;

    void opAssign()(T value)
    {
        assert(0);
    }

    @property ref inout(T) get() inout
    {
        assert(0);
    }
    alias get this;
}

void test11730()
{
    Nullable11730!SysTime11730[string] map;
    map["foo"] = Nullable11730!SysTime11730();
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14089

struct S14089
{
    int num;
    S14089 opAssign(S14089 val) { return this; }
}

void test14089()
{
    S14089[int] aa;
    S14089 b = aa[1] = S14089(0);
    assert(aa[1].num == 0);
    assert(b.num == 0);
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14144

struct JSON14144
{
    union
    {
        double _floating;
    }

    this(typeof(null))
    {
    }

    @trusted pure nothrow typeof(null) opAssign(typeof(null) nothing)
    {
        return null;
    }
}

void test14144()
{
    JSON14144[string] x;
    x["wat"] = null;
    assert(x.length == 1);
    assert("wat" in x);
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14321

void test14321()
{
    struct Foo
    {
        static char[8] buf;
        static char[] op;

        this(int id) { buf[op.length] = 'c'; op = buf[0..op.length + 1]; }
        this(this) { buf[op.length] = 'p'; op = buf[0..op.length + 1]; }
        ~this() { buf[op.length] = 'd'; op = buf[0..op.length + 1]; }
    }
    Foo[string] foos;
    assert(Foo.op == "");
    foos["test"] = Foo(42);     // initialization
    assert(Foo.op == "c");
    foos["test"] = Foo(42);     // assignment
    assert(Foo.op == "ccd");

    struct Bar
    {
        static char[8] buf;
        static char[] op;

        int id;
        //this(int id) { op ~= "c"; }
        this(this) { buf[op.length] = 'p'; op = buf[0..op.length + 1]; }
        ~this() { buf[op.length] = 'd'; op = buf[0..op.length + 1]; }
    }
    Bar[string] bars;
    assert(Bar.op == "");
    bars["test"] = Bar(42);     // initialization
    assert(Bar.op == "");
    bars["test"] = Bar(42);     // assignment
    assert(Bar.op == "d");
}

/************************************************/
// https://issues.dlang.org/show_bug.cgi?id=19112

void test19112()
{
    int[int[1]] aa;
    aa[[2]] = 1;
    assert([2] in aa);

    int[int[]] aa2 = [[1, 2, 3]: 4];
    int[3] k = [1, 2, 3];
    assert(*(k in aa2) == 4);
}

/************************************************/

int main()
{
    printf("before test 1\n");   test1();
    printf("before test 2\n");   test2();
    printf("before test 4\n");   test4();
    printf("before test 5\n");   test5();
    printf("before test 6\n");   test6();
    printf("before test 7\n");   test7();
    printf("before test 8\n");   test8();
    printf("before test 9\n");   test9();
    printf("before test 10\n");   test10();
    printf("before test 11\n");   test11();
    printf("before test 12\n");   test12();
    printf("before test 13\n");   test13();
    printf("before test 14\n");   test14();
    printf("before test 15\n");   test15();
    printf("before test 16\n");   test16();
    printf("before test 17\n");   test17();
    printf("before test 18\n");   test18();
    printf("before test 19\n");   test19();
    printf("before test 20\n");   test20();
    printf("before test 21\n");   test21();
    printf("before test 22\n");   test22();
    printf("before test 23\n");   test23();
    printf("before test 24\n");   test24();
    printf("before test 25\n");   test25();
    printf("before test 26\n");   test26();
    printf("before test 27\n");   test27();
    printf("before test 28\n");   test28();
    printf("before test 29\n");   test29();
    printf("before test 30\n");   test30();
    printf("before test 31\n");   test31();
    printf("before test 32\n");   test32();

    test4826c();
    test5131();
    test6178();
    test6178x();
    test10595();
    test10970();
    test6433();
    test6612();
    test7365();
    test5520();
    test6799();
    test11359();
    test11730();
    test14089();
    test14321();
    test19112();

    printf("Success\n");
    return 0;
}
