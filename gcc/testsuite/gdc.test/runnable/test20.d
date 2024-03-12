import core.vararg;

extern(C) int printf(const char*, ...);

/*****************************************/

class A1
{
    union
    {
        struct
        {
            int x;
            public int y;
        }
        protected int z;
    }
}

class A2
{
    struct
    {
        int x;
        public int y;
    }
}

class A3
{
    union
    {
        int x;
        public int y;
    }
}

void test1()
{
    A1 a1 = new A1();
    A2 a2 = new A2();
    A3 a3 = new A3();

    a1.x = 1;
    a1.y = 2;
    a1.z = 3;
    assert(a1.x == 3);
    assert(a1.y == 2);

    a2.x = 1;
    a2.y = 2;
    assert(a2.x == 1);

    a3.x = 1;
    a3.y = 2;
    assert(a3.x == 2);
}

/*****************************************/

struct A4
{
    union
    {
        struct
        {
            int x = 13;
        }
        int y;
    }
}

void test4()
{
    printf("A4.sizeof = %zd\n", A4.sizeof);
    assert(A4.sizeof == 1 * int.sizeof);

    A4 q;
    assert(q.y == 13);
}

/*****************************************/

class A5
{
     union {
         struct {
             int x;
         }
         public int y;
     }
}

void test5()
{
     A5 a = new A5;

     a.x = 3;
     a.y = 4;
     assert(a.x == 4);
}

/*****************************************/

int i6 = 5;

float m6 = 5.0;

void test6()
{
  void f()
  {
    void i6(float j)
    {
        m6 = j;
    }

    void g()
    {
        i6 = 1;
    }
    g();
  }

  f();

  printf( "%d %f\n", i6, m6);
  assert(i6 == 5);
  assert(m6 == 1);
}

/*****************************************/

const int a1 = 50;
const int a2 = 50;
const int a3 = a1 * a2;

const int b1 = a1 - 1;
const int b2 = a2 - 1;

const int c1 = 50*50;
const int c2 = (a1-1)*(a2-1);
const int c3 = b1*b2;

int[4*c1]  array1;  // illegal!
int[4*c2]  array2;  // illegal!
int[4*c3]  array3;  // illegal!

int[a3]    array4;  // valid! no error!

void test7()
{
    assert(a1 == 50);
    assert(a2 == 50);
    assert(a3 == 2500);

    assert(b1 == 49);
    assert(b2 == 49);

    assert(c1 == 2500);
    assert(c2 == 2401);
    assert(c3 == 2401);

    assert(array1.length == 10000);
    assert(array2.length == 9604);
    assert(array3.length == 9604);
}

/*****************************************/

struct Foo8
{
    static Foo8 bar()
    {
        Foo8 res;
        return res;
    }
}

void test8()
{
    Foo8[8] x;
    x[] = Foo8.bar;
}


/*****************************************/

void test9()
{
     try {
     } catch (Exception e) {
         debug printf("Exception happened\n");
     }
}

/*****************************************/

struct Foo10 {
  const bool opEquals(const ref Foo10 x) {
    return this.normalize is x.normalize;
  }
  const Foo10 normalize() {
    Foo10 res;
    return res;
  }
}

void test10()
{
}


/*****************************************/

class T11
{
    this(){}
    ~this(){}
}

void test11()
{
    scope T11 t=new T11();
    int i=1;
    switch(i)
    {
        case 1:
            break;

        default:
            break;
    }
}

/*****************************************/

void test12()
{
        char[] s;
        char[] t;

        if (true)
            s = null;
        s = (true) ? null : t;
        t = (true) ? s : null;
}

/*****************************************/

class Foo13
{
       int init (int x) { return 1; }

       static int init (long y) { return 2; }
}

void test13()
{
    Foo13 f = new Foo13();
    int i;

    i = f.init(1);
    assert(i == 1);
    i = f.init(1L);
    assert(i == 2);
}

/*****************************************/

void write14(bool[] c)
{
    printf("[%2zd]: ", c.length);
    foreach (bool x; c)
        printf("%d,", x);
    printf("\n");
}

void test14()
{
     static bool[] a = [ 1, 1, 0, 1, 0 ];
     static bool[] b = [ 1, 0, 0, 1 ];
     bool[] c = a ~ b;

     static bool[] r1 = [1,1,0,1,0,1,0,0,1];
     static bool[] r2 = [1,1,0,1,0,1,0,0,1,1,1,0,1,0];
     static bool[] r3 = [1,1,0,1,0,1,0,0,1,1,1,0,1,0,0];
     static bool[] r4 = [1,1,0,1,0,1,0,0,1,1,1,0,1,0,0,1];


     write14(c);
     assert(c == r1);

     c ~= a;
     write14(c);
     assert(c == r2);

     c ~= 0;
     write14(c);
     assert(c == r3);

     c ~= 1;
     write14(c);
     assert(c == r4);
}

/*****************************************/

void test15()
{
        bool[] b;
        bool[] c;

        b.length = 10;
        c = b[0..4];
        c[] = true;

        assert(b[0] == true);
        assert(b[1] == true);
        assert(b[2] == true);
        assert(b[3] == true);

        assert(b[4] == false);
        assert(b[5] == false);
        assert(b[6] == false);
        assert(b[7] == false);
        assert(b[8] == false);
        assert(b[9] == false);
}

/*****************************************/

int y16;

class C16
{
        int x;
        this()
        {
            x = 4;
        }
}

void test16()
{
    C16 c = new C16();
    assert(c.x == 4);
}

/*****************************************/

ubyte* ptr17;

void test17()
{
    ubyte[16] foo;

    printf("foo = %p\n", foo.ptr);
    ptr17 = foo.ptr;
    abc17(foo);
}

void abc17(ref ubyte[16] bar)
{
    printf("bar = %p\n", bar.ptr);
    assert(bar.ptr == ptr17);
}

/*****************************************/

struct Iterator18(T)
{
   T* m_ptr;

   const bool opEquals(const ref Iterator18 iter)
   {
     return (m_ptr == iter.m_ptr);
   }

   const int opCmp(const ref Iterator18 iter)
   {
     return cast(int)(m_ptr - iter.m_ptr);
   }
}

void test18()
{
    Iterator18!(int) iter;
}

/*****************************************/

struct S29(T)
{
   const bool opEquals(const ref S29!(T) len2)
   {
     return 0;
   }

   const int opCmp(const ref S29!(T) len2)
   {
     return 0;
   }
}


void test19()
{
   TypeInfo info = typeid(S29!(int));
}


/*****************************************/

class Mapped : Buffer
{
        this()
        {
        }

        ~this()
        {
        }
}

class Buffer
{
        private uint limit;
        private uint capacity;
        private uint position;

        invariant()
        {
               assert (position <= limit);
               assert (limit <= capacity);
        }
}

void test20()
{
    Buffer b = new Buffer();
    destroy(b);
}

/*****************************************/

class T21
{
 char[1] p1;
 char[1] p2;
public:
 char[] P() {return p1~p2;}
}

void test21()
{
    T21 t = new T21;
    t.p1[0] = 'a';
    t.p2[0] = 'b';
    assert(t.P() == "ab");
}

/*****************************************/

void test22()
{
    struct foo1 { int a; int b; int c; }

    class foo2
    {
        foo1[] x;

        foo1 fooret(foo1 foo2)
        {
            x.length = 9;
            return x[0] = foo2; // Here

            // x[0] = foo2; --- This version does not
            // return x[0]; --- cause the error.
        }
    }
}


/*****************************************/

interface I24 { }

void test24()
{
}

/*****************************************/

interface D25{}
interface C25:D25{}
interface B25:C25{}
interface A25:B25{}

void test25()
{
}

class     A26:B26{}
interface B26:C26{}
interface C26:D26{}
interface D26{}

void test26()
{
}

interface A27:B27{}
interface B27:C27{}
interface C27:D27{}
interface D27{}

void test27()
{
}

/*****************************************/

void test28()
{
    const double d = -1.0;
    int i = cast(int)d;
    printf("i = %d\n", i);
    assert(-1 == i);
}

/*****************************************/

static int[1][5] array = [[1],[2],[3],[4],[5] ];

void Lookup( int which )
{
   switch( which )
   {
     case 0 : return cast(void)array[which];
     default: assert(0);
   }
}

void test29()
{
}

/*****************************************/

void foo31(...)
{
    byte b = va_arg!byte(_argptr);
    assert(b == 8);
}

void test31()
{
    byte x = 9;
    foo31(--x);
    --x;
    foo31(++x);
}

/*****************************************/

template Foo33(T, int L)
{
        T[L] arr;
        class Bar {
                int before = 6;
                T[L] arr;
                int after = 7;
        }
}

void test33()
{
        alias Foo33!(int, 100) foo;
        foreach (int x; foo.arr)
                assert(x == int.init);

        foo.Bar bar = new foo.Bar();
        foreach (int x; bar.arr)
        {       //printf("%d\n", x);
                assert(x == int.init);
        }
}


/*****************************************/

void test34()
{
        bool[1] a;
        bool[1] b;

        bool[] concat() {
                return a~b;
        }

        a[]=0;
        b[]=1;

        bool[] arr=concat();

        assert(arr.length==2);
        assert(arr[0]==0);
        assert(arr[1]==1);
}

/*****************************************/

void dummy35(...) {}

void test35()
{
    byte x = 9;
    dummy35(x);
    int y = --x;
    assert (y == 8);
    assert (x == 8);
}

/*****************************************/

void test36()
{
        int[] a;
        a.length = 2;
        a[0]=1;
        a[1]=2;

        int[] b;
        b.length = 1;
        b[0]=3;

        a~=b;

        assert(a.length==3);
        assert(a[0]==1);
        assert(a[1]==2);
        assert(a[2]==3);

        assert(b.length==1);
        assert(b[0]==3);
}

/*****************************************/

struct Range{
        int width(){
                return 1;
        }
}

class Container {
        Range opIndex(int i){
                return data[i];
        }

        Range[2] data;
}

void test37()
{
        Container ranges=new Container;

// fails with -inline
//      assert(ranges[0].width() == 1);
}

/*****************************************/

void test38()
{
        uint mask = (uint.max >> 1);
        assert(mask == (uint.max >> 1));
}



/*****************************************/

void test41()
{
    assert(new void[40] == new void[40]);
}

/*****************************************/

void test42()
{
        static ubyte[] master = [
                0xE3u, 0x83u, 0xAFu, 0xE3u, 0x83u, 0xADu, 0xE3u, 0x82u,
                0xB9u, 0xEFu, 0xBDu, 0x97u
        ];

        string string1 =  "ワロスｗ";
        string string2 = r"ワロスｗ";
        string string3 =  `ワロスｗ`;
        string string4 = "\xE3\x83\xAF\xE3\x83\xAD\xE3\x82\xB9\xEF\xBD\x97";

        assert(string1.length==master.length);

        for(int i=0; i<master.length; i++){
                assert(string1[i]==master[i]);
        }

        assert(string2.length==master.length);

        for(int i=0; i<master.length; i++){
                assert(string2[i]==master[i]);
        }

        assert(string3.length==master.length);

        for(int i=0; i<master.length; i++){
                assert(string3[i]==master[i]);
        }

        assert(string4.length==master.length);

        for(int i=0; i<master.length; i++){
                assert(string4[i]==master[i]);
        }
}

/*****************************************/

struct S43
{
        int i=1;
}

static S43[3] s= [ 1: { 2 } ];

void test43()
{
        assert(s[0].i==1);
        assert(s[1].i==2);
        assert(s[2].i==1);
}

/*****************************************/

struct S44
{
  int i;
}

static S44[12] S44s = [];

void test44()
{
}


/*****************************************/

struct S45
{
    double x = 0, y = 0;
    static S45 opCall(int i) { S45 r; r.x = i; return r; }
    S45 opBinary(string op)(S45 s) if (op == "*")
    {
        S45 r;
        r.x = x * s.x;
        r.y = y * s.y;
        return r;
    }
}

template sqr(F) { F sqr(F x) { return x * x; } }

template pow(F)
{
    F pow(F x, int i) {
        if (i < 1)
        {
            static if (is(F : real))
                return 1;
            else
                return F(1);
        }
        if (i & 1)
        {
            if (i == 1)
                return x;
            else
                return x * pow(x,i-1);
        }
        return sqr!(F)(pow(x,i/2));
    }
}

void test45()
{
    S45 s = S45(10);
    S45 val = pow!(S45)(s,2);
    printf("x = %2.2f, y = %2.2f\n", val.x, val.y);
    assert(val.x == 100);
    assert(val.y == 0);
    double d = pow!(double)(10,3);
    printf("%2.2f\n", d);
    assert(d == 1000);
}

/*****************************************/

struct Node46
{
  Node46* left,right,parent;
  int color = 5;
  char[] key;
  char[] value;
}

void test46()
{
  Node46* x = new Node46;
  assert( x.left is null );
  assert( x.right is null );
  assert( x.parent is null );
  assert( x.color is 5 );
  assert( x.key is null );
  assert( x.value is null );
}

/*****************************************/

string foo48(string s)
{
    return s;
}

void test48()
{
    char c = '?';
    string s;

    s = foo48("is this it" ~ c ~ " yep!");
    assert(s == "is this it? yep!");

    s = foo48("is this it" ~ c);
    assert(s == "is this it?");

    s = foo48(c ~ "is this it");
    assert(s == "?is this it");
}


/*****************************************/

char[10] foo49 = void;

void test49()
{
    int i = void;
    //printf("i = %d\n", i);
    int[10] a;
    foreach (int x; a)
    {
        printf("\tx = %d\n", x);
    }
}

/*****************************************/

void foo50(string s) { assert(s == "is this it? yep!"); }

void test50()
{
    char c = '?';
    foo50("is this it"~c~" yep!");
}

/*****************************************/

void test51()
{
    bool[9][3] qwert;

    printf("qwert.sizeof = %zd\n", qwert.sizeof);

    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 9; j++)
            assert(qwert[i][j] == false);

    version (D_Bits)
        assert(qwert.sizeof == 12);
    else
        assert(qwert.sizeof == 27);

    qwert[0][3] = true;
    qwert[0][5] = true;
    qwert[0][7] = true;

    qwert[1..3] = qwert[0];

    for (int i = 0; i < 3; i++)
    {
        assert(qwert[i][0] == false);
        assert(qwert[i][1] == false);
        assert(qwert[i][2] == false);
        assert(qwert[i][3] == true);
        assert(qwert[i][4] == false);
        assert(qwert[i][5] == true);
        assert(qwert[i][6] == false);
        assert(qwert[i][7] == true);
        assert(qwert[i][8] == false);
    }
}


/*****************************************/

void test52()
{
    size_t vsize = void.sizeof;
    assert(vsize == 1);
}

/*****************************************/

const char[3][13] month = [
     1: "Jan", "Feb", "Mar", "Apr", "May", "Jun",
     8: "Aug", "Sep", "Oct", "Nov", "Dec"
];

void test53()
{
    printf("%.*s\n", cast(int)month[1].length, month[1].ptr);
    printf("%.*s\n", cast(int)month[2].length, month[2].ptr);
    printf("%.*s\n", cast(int)month[3].length, month[3].ptr);
    printf("%.*s\n", cast(int)month[4].length, month[4].ptr);
    printf("%.*s\n", cast(int)month[5].length, month[5].ptr);
    printf("%.*s\n", cast(int)month[6].length, month[6].ptr);
    printf("%.*s\n", cast(int)month[8].length, month[8].ptr);

    assert(month[1] == "Jan");
    assert(month[2] == "Feb");
    assert(month[3] == "Mar");
    assert(month[4] == "Apr");
    assert(month[5] == "May");
    assert(month[6] == "Jun");
    assert(month[8] == "Aug");
    assert(month[9] == "Sep");
    assert(month[10] == "Oct");
    assert(month[11] == "Nov");
    assert(month[12] == "Dec");

    assert(month[0][0] == 0xFF);
    assert(month[0][1] == 0xFF);
    assert(month[0][2] == 0xFF);

    assert(month[7][0] == 0xFF);
    assert(month[7][1] == 0xFF);
    assert(month[7][2] == 0xFF);
}

/*****************************************/

struct S54
{
    static S54 foo()
    {
        S54 s;
        with (s) {} // bug
        return s;
    }

    static S54 bar()
    {
        return S54.foo() * S54.foo();
    }

    S54 opBinary(string op)(S54 s) if (op == "*")
    {
        return s;
    }
}

void test54()
{
    S54.bar();
}

/*****************************************/

void test55()
{
  char c = 'a';
  char[] str, uvw;
  str = str ~ c;
  uvw = c ~ uvw;

  printf("%.*s\n", cast(int)str.length, str.ptr);
  assert(str == "a");
  assert(uvw == "a");

  c = 'b';
  printf("%.*s\n", cast(int)str.length, str.ptr);
  assert(str == "a");
  assert(uvw == "a");
}

/*****************************************/

void test56()
{
    char[4] a, b;
    a[] = 'a'; b[] = 'a';

    for (int i = 0; i < 4; i++)
    {
        assert(a[i] == 'a');
        assert(b[i] == 'a');
    }

    a[] = b[] = 'b';

    for (int i = 0; i < 4; i++)
    {
        assert(a[i] == 'b');
        assert(b[i] == 'b');
    }

}


/*****************************************/

void test57()
{
        char[] a;
        char[] b;

        a = a ~ 'x';
        assert(a == "x");
        b = 'c' ~ a ~ 'b';
        assert(b == "cxb");
}


/*****************************************/

struct Foo58
{
    ubyte a, b, c;
}

void test58()
{
    Foo58 d;
    foobar58(d);
    return cast(void)0;
}

void foobar58(Foo58 e) {}


/*****************************************/

void foo59(string s)
{
    assert(s == "hello");
}

void foo59(wstring s)
{
    assert(s == "baby");
}

void foo59(dstring s)
{
    assert(s == "jane");
}

void test59()
{
    string h = "hello"c;
    wstring b = "baby"w;
    dstring j = "jane"d;

    //writefln("%s %s %s", h, b, j);

    foo59(h);
    foo59("hello"c);
    foo59(b);
    foo59("baby"w);
    foo59(j);
    foo59("jane"d);
}

/*****************************************/

class C60
{
    static const int x;
    const int y;

    void foo()
    {
    }

    shared static this()
    {
        x = 5;
    }

    this()
    {
        y = 7;
    }
}

const int z60;

shared static this()
{
    z60 = 3;
}

void test60()
{
    C60 c = new C60();

    assert(c.x == 5);
    assert(c.y == 7);
    assert(z60 == 3);
}

/*****************************************/

void foo61(real[] arr)
{
    // i = 0 doesn't trigger the bug, everything bigger seems to
    // also, making this const fixes the bug
    size_t i = 1;

    for (size_t j = i; j >= i; j -= i)
    {
        // interesting results follow from this:
        printf("%zd ", i);
        // it prints a _lot_ of ones

        arr[j] = arr[j - i];
    }
}

void test61()
{
    real[] array;
    array.length = 2; // whatever, as long as it's more than 1

    foreach (ref real i; array)
        i = 1; // just something

    foo61(array);
}

/*****************************************/

void bug7493()
{
    string str = "abcde";
    const(void) [][1] arr = [str];
    assert(arr[0].length == str.length);
    const(void) [][1] arr2;
    arr2 = [str];
    assert(arr[0].length == str.length);
}

/*****************************************/

int main()
{
    test1();
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
    test24();
    test25();
    test26();
    test27();
    test28();
    test29();
    test31();
    test33();
    test34();
    test35();
    test36();
    test37();
    test38();
    test41();
    test42();
    test43();
    test44();
    test45();
    test46();
    test48();
    test49();
    test50();
    test51();
    test52();
    test53();
    test54();
    test55();
    test56();
    test57();
    test58();
    test59();
    test60();
    test61();
    bug7493();

    printf("Success\n");
    return 0;
}
