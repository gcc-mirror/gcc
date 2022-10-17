// PERMUTE_ARGS: -unittest -O -release -inline -fPIC -g

extern(C) int printf(const char*, ...);
extern(C) int sprintf(char*, const char*, ...);

/**************************************/

void test1()
{
     bool x;
     int i;

     i = !("bar" == "bar");
     assert(i == 0);

     i = "bar" != "bar";
     assert(i == 0);

     i = "bar" == "bar";
     assert(i == 1);

     x = "bar" != "bar";
     assert(x == false);

     assert("bar" == "bar");

     x = "bar" == "bar";
     assert(x == true);

     /+ ---- +/

     i = !("foo" == "bar");
     assert(i == 1);

     i = "foo" != "bar";
     assert(i == 1);

     i = "foo" == "bar";
     assert(i == 0);

     x = "foo" != "bar";
     assert(x == true);

     assert("foo" != "bar");

     x = "foo" == "bar";
     assert(x == false);

}


/**************************************/

void test2()
{
     bool x;
     int i;

     i = !("bar" <= "bar");
     assert(i <= 0);

     i = "bar" > "bar";
     assert(i == 0);

     i = "bar" >= "bar";
     assert(i == 1);

     x = "bar" < "bar";
     assert(x == false);

     assert("bar" <= "bar");

     x = "bar" <= "bar";
     assert(x == true);

     /+ ---- +/

     i = !("foo" < "bar");
     assert(i == 1);

     i = "foo" > "bar";
     assert(i == 1);

     i = "foo" < "bar";
     assert(i == 0);

     x = "foo" >= "bar";
     assert(x == true);

     assert("foo" >= "bar");

     x = "foo" <= "bar";
     assert(x == false);

}


/**************************************/

bool all(string array, bool function(char) predicate) {
    for (int i = 0; i < array.length; i++) {
        if (!predicate(array[i])) {
            return false;
        }
    }
    return true;
}

bool all(string array, bool delegate(char) predicate) {
    for (int i = 0; i < array.length; i++) {
        if (!predicate(array[i])) {
            return false;
        }
    }
    return true;
}

bool isVowel(char c) {
    return (c == 'a') || (c == 'e') || (c == 'i') || (c == 'o') || (c == 'u');
}

class Character {
    private char letter;
    this(char c) {
        this.letter = c;
    }
    public bool isLetter(char c) {
        return this.letter == c;
    }
}

void test3()
{
    Character a = new Character('a');
    bool delegate(char) isLetter;
    isLetter = &a.isLetter;
    bool i;

    i = all("aeiouoeieuiei", &isVowel);
    assert(i == true);

    i = all("aeiouoeieuiei", isLetter);
    assert(i == false);
}


/**************************************/

int[] fun(int i)
    in
    {
        assert(i > 0);
    }
    out (result)
    {
        assert(result[0] == 2);
    }
    do
    {
        char result;
        int[] res = new int[10];
        res[] = i;
        int isZero = (result == 0xFF);
        assert(isZero);
        return res;
    }

void test4()
{
    int[] values = fun(2);
}


/**************************************/


const uint D3DSP_DSTMOD_SHIFT = 20;

const uint D3DSP_DSTMOD_MASK = 0x00F00000;


enum D3DSHADER_PARAM_DSTMOD_TYPE

{
    NONE = 0<<D3DSP_DSTMOD_SHIFT,
    SATURATE= 1<<D3DSP_DSTMOD_SHIFT,
    FORCE_DWORD = 0x7fffffff,
}

void test5()
{
}

/**************************************/

class foo6
{
     union
     {
         int i;
         char j;
     }
}

void test6()
{
}

/**************************************/

void test7()
{
}


/**************************************/

real x8 = 4;
long y8;

void test8()
{
    y8 = cast(long)x8;
    assert(y8 == 4);
    y8 = cast(long)cast(double)x8;
    assert(y8 == 4);
    printf ("%lld\n", y8);
}


/**************************************/

void test9()
{
    struct B { }
    B bar (ref int p)
    {
        B b;
        return b;
    }
}


/**************************************/

struct B10
{
   uint member;
}

B10 func10()
{
   B10 b;
   b.member=7;
   return b;
}

void test10()
{
   uint i=func10().member; // Internal error: ..\ztc\cgcs.c 350
   assert(i == 7);

   B10 b=func10();  //works
   i=b.member;
   assert(i == 7);
}


/**************************************/

void test11()
{
   const int notInitialized;
   const int i=notInitialized; // DMD crashes
   assert(notInitialized == 0);
   assert(i == 0);
}


/**************************************/

struct Array12
{
    char len;
    void* p;
}
//Array12 f12(string a)
//{
//    return *cast(Array12*) &a[0..23]; //Internal error: ..\ztc\cgcs.c 350
//}

Array12 g12(string a)
{
    return *cast(Array12*) &a; //works
}

void test12()
{
    string a = "12345678901234567890123";
    Array12 b;

    //b = f12(a);
    //printf("b.len = %x\n", b.len);
    b = g12(a);
    printf("b.len = %x\n", b.len);
}


/**************************************/


class A13
{
    int opBinary(string op : "<<")(char* v) { return 1; }
    int opBinary(string op : "<<")(string v) { return 2; }
}

void test13()
{
        A13 a = new A13();
        int i;
        i = a << cast(string) "";
        assert(i == 2);
        i = a << cast(string)"";
        assert(i == 2);
}

/**************************************/

union U1
{
   struct { int a; }
   struct { int b; }
}

union U2
{
   struct { int a; }
   struct { long b; }
}

union U3
{
   struct { long a; }
   struct { int b; }
}

union U4
{
   int a;
   int b;
}

union U5
{
   int a;
   long b;
}

union U6
{
   long a;
   int b;
}

void test14()
{
   printf("%zd %zd %zd\n", U1.a.offsetof, U1.b.offsetof, U1.sizeof);
   assert(U1.a.offsetof == 0);
   assert(U1.b.offsetof == 0);
   assert(U1.sizeof == 4);

   printf("%zd %zd %zd\n", U2.a.offsetof, U2.b.offsetof, U2.sizeof);
   assert(U2.a.offsetof == 0);
   assert(U2.b.offsetof == 0);
   assert(U2.sizeof == 8);

   printf("%zd %zd %zd\n", U3.a.offsetof, U3.b.offsetof, U3.sizeof);
   assert(U3.a.offsetof == 0);
   assert(U3.b.offsetof == 0);
   assert(U3.sizeof == 8);

   printf("%zd %zd %zd\n", U4.a.offsetof, U4.b.offsetof, U4.sizeof);
   assert(U4.a.offsetof == 0);
   assert(U4.b.offsetof == 0);
   assert(U4.sizeof == 4);

   printf("%zd %zd %zd\n", U5.a.offsetof, U5.b.offsetof, U5.sizeof);
   assert(U5.a.offsetof == 0);
   assert(U5.b.offsetof == 0);
   assert(U5.sizeof == 8);

   printf("%zd %zd %zd\n", U6.a.offsetof, U6.b.offsetof, U6.sizeof);
   assert(U6.a.offsetof == 0);
   assert(U6.b.offsetof == 0);
   assert(U6.sizeof == 8);
}


/**************************************/

void test15()
{
    const int[] array = [1,2,3];

    assert(array.length == 3);
    assert(array[1] == 2);
}


/**************************************/

class Conversion(T)
{
  class Big {char[2] dummy;}
  static Big Test(...);

  enum { exists = Test().sizeof }
}

void test16()
{
    assert(Conversion!(double).exists == (void*).sizeof);
}


/**************************************/

class Cout17
{
 Cout17 set(int x)
 {
  printf("%d",x);
  return this;
 }
 alias opBinary(string op : "<<") = set;
}

void test17()
{
 Cout17 cout = new Cout17;
 cout << 5 << 4;
}



/**************************************/

void test18()
{
  bool[] x = new bool[100];
  x[] = 1;
  bool[] y = x.dup;
  assert(y[99]);
}


/**************************************/

bool[32] x19 = 1;

void test19()
{
    assert(x19[31]);
}


/**************************************/

bool[2] y20 = [ cast(bool) 3, cast(bool) 2 ];

void test20()
{
    assert(y20[0]); // ok
    assert(y20[1]); // fails
}

/**************************************/

bool isnan(float x) { return !( (x >= 0)  || (x < 0)); }

struct X21 { float f, g, h; }
X21 x21_1;
X21 x21_2 = { f: 1.0, h: 2.0 };

char[3] y21_1;
char[3] y21_2 = [ 0: 'a', 2: 'b' ];

void test21()
{
    assert(isnan(x21_1.g));
    assert(isnan(x21_2.g));
    assert(y21_1[1] == '\xff');
    assert(y21_2[1] == '\xff');
}


/**************************************/

void test22()
{
    wstring a = cast(wstring)"一〇";
}


/**************************************/

interface A23 { void x(); }
class B23 : A23 { void x() { } }
class C23 : B23 { uint y = 12345678; }

void stest23(A23 a)
{
    synchronized (a)
    {
    }
}

void test23()
{
    C23 c = new C23;
    assert(c.y == 12345678 /*c.y.init*/);
    stest23(c);
    assert(c.y == 12345678 /*c.y.init*/);
}


/**************************************/

class A24
{
    unittest
    {
    }
}

void test24()
{
}

/**************************************/

char rot13(char ret)
{
    if (ret > 'A'-1 && ret < 'N')
    {   ret += 13;}
    else if(ret > 'M' && ret < 'Z'+1)
    {   ret -= 13;}
    else if(ret > 'a'-1 && ret < 'n')
    {   ret += 13;}
    else if(ret > 'm' && ret < 'z'+1)
    {   ret -= 13;}

    return ret;
}


void test25()
{
    foreach (char c; "hello World\n")
        printf("%c %c\n", c, rot13(c));
    assert(rot13('h') == 'u');
    assert(rot13('o') == 'b');
    assert(rot13('W') == 'J');
    assert(rot13('H') == 'U');
}


/**************************************/

bool b26a = cast(bool)( cast(bool) 2 & cast(bool) 1 );
bool b26b = cast(bool) 2;

void test26()
{
    assert( (* cast(byte *) & b26a) == 1 );
    assert( (* cast(byte *) & b26b) == 1 );
}


/**************************************/

int c27;

struct X27
{
  int x;
  struct
  {
      int a;
      int b;
      static this() { c27 = 3; }
  }
}

void test27()
{
    assert(c27 == 3);
}

/**************************************/

void test28()
{
  void bar()
  {
    throw new Foo28();
  }
}

class Foo28 : Throwable
{
  private:
        this() { super(""); }
}

/**************************************/

struct S29 {
    ubyte a, b, c, d;
}

int hoge(S29 s) {
    char[10] b;
    printf("%x\n", *cast(int*)&s);
    sprintf(b.ptr, "%x", *cast(int*)&s);
    version (LittleEndian)
        assert(b[0 .. 7] == "4030201");
    version (BigEndian)
        assert(b[0 .. 7] == "1020304");
    return 0;
}

void test29()
{
    for (int i = 0; i < 1; i++) {
        S29 s;
        s.a = 1;
        s.b = 2;
        s.c = 3;
        s.d = 4;
        hoge(s);
    }
}


/**************************************/

class Qwert {
     static {
         deprecated int yuiop() {
             return 42;
         }
     }

     static deprecated int asdfg() {
         return yuiop() + 105;
     }
}

void test30()
{
}

/**************************************/

void test31()
{
    string foo = "hello";

    printf("%s\n", foo.ptr);
    auto s = typeid(typeof(foo.ptr)).toString();
    printf("%.*s\n", cast(int)s.length, s.ptr);
    s = typeid(char*).toString();
    printf("%.*s\n", cast(int)s.length, s.ptr);
    assert(typeid(typeof(foo.ptr)) == typeid(immutable(char)*));
}

/**************************************/

class Qwert32
{
    struct
    {
        int yuiop = 13;
    }
    int asdfg = 42;

    void foo()
    {
        printf("yuiop = %zd, asdfg = %zd\n", Qwert32.yuiop.offsetof, Qwert32.asdfg.offsetof);
        version(D_LP64)
        {
            assert(Qwert32.yuiop.offsetof == 16);
            assert(Qwert32.asdfg.offsetof == 20);
        }
        else
        {
            assert(Qwert32.yuiop.offsetof == 8);
            assert(Qwert32.asdfg.offsetof == 12);
        }
    }
}

void test32()
{
    Qwert32 q = new Qwert32;

    q.foo();
}


/**************************************/

int x33;
int y33;

size_t os_query()
{
    return cast(uint)(cast(char *)&x33 - cast(char *)&y33);
}

void test33()
{
    os_query();
}

/**************************************/

uint x34 = ~(16u-1u);
uint y34 = ~(16u-1);

void test34()
{
     assert(x34 == 0xFFFFFFF0);
     assert(y34 == 0xFFFFFFF0);
}

/**************************************/

private static extern (C)
{
        shared char* function () uloc_getDefault;
}


static shared void**[] targets =
    [
        cast(shared(void*)*) &uloc_getDefault,
    ];

void test35()
{
}

/**************************************/

class S36
{
    int s = 1;

    this()
    {
    }
}


class A36 : S36
{
    int a = 2;
    int b = 3;
    int c = 4;
    int d = 5;
}

void test36()
{
    A36 a = new A36;

    printf("A36.sizeof = %zd\n", a.classinfo.initializer.length);
    printf("%d\n", a.s);
    printf("%d\n", a.a);
    printf("%d\n", a.b);
    printf("%d\n", a.c);
    printf("%d\n", a.d);

    version(D_LP64)
        assert(a.classinfo.initializer.length == 36);
    else
        assert(a.classinfo.initializer.length == 28);
    assert(a.s == 1);
    assert(a.a == 2);
    assert(a.b == 3);
    assert(a.c == 4);
    assert(a.d == 5);
}


/**************************************/

struct MyStruct
{
    StructAlias* MyStruct()
    {
        return null;
    }
}

alias MyStruct StructAlias;

void test37()
{
}

/**************************************/

class Foo38
{
    static void display_name()
    {
        printf("%.*s\n", cast(int)Object.classinfo.name.length, Object.classinfo.name.ptr);
        assert(Object.classinfo.name == "object.Object");
        assert(super.classinfo.name == "object.Object");
        assert(this.classinfo.name == "test12.Foo38");
    }
}

void test38()
{
    Foo38.display_name();
}


/**************************************/
// https://www.digitalmars.com/d/archives/digitalmars/D/bugs/2409.html

class C39
{
    C39 c;
    this() { c = this; }
    C39 lock() { return c; }
}

void test39()
{
    C39 c = new C39();
    synchronized( c.lock() ) {}
    synchronized( c.lock ) {}
}


/**************************************/

class C40
{
    static int x = 4;

    static int foo()
    {
        return this.x;
    }
}

void test40()
{
    C40 c = new C40();
    assert(C40.foo() == 4);
}


/**************************************/

struct Foo42
{
    Bar42 b;
}

struct Bar42
{
    long a;
}

void test42()
{
    assert(Bar42.sizeof == long.sizeof);
    assert(Foo42.sizeof == long.sizeof);
}


/**************************************/

class Foo43
{
    Bar43 b;
}

struct Bar43
{
    long a;
}

void test43()
{
    assert(Bar43.sizeof == long.sizeof);
    assert(Foo43.sizeof == (void*).sizeof);
}


/**************************************/

struct Property
{
    uint attributes;

    Value value;
}

struct Value
{
    int a,b,c,d;
}

struct PropTable
{
    Property[Value] table;
    PropTable* previous;

    Value* get(Value* key)
    {
        Property *p;

        p = *key in table;
        p = &table[*key];
        table.remove(*key);
        return null;
    }

}

void test44()
{
}


/**************************************/

struct Shell
{
    string str;

    const int opCmp(ref const Shell s)
    {
        // Obviously not Unicode-aware...
        foreach (const i, const a; this.str)
        {
            const b = s.str[i];
            if (a < b) return -1;
            if (a > b) return 1;
        }

        if (this.str.length < s.str.length) return -1;
        if (this.str.length > s.str.length) return  1;
        return 0;
    }
}

void test45()
{
    Shell a = Shell("hello");
    Shell b = Shell("betty");
    Shell c = Shell("fred");

    assert(a > b);
    assert(b < c);
}

/**************************************/

class A46
{
    char foo() { return 'a'; }
}

class B46 : A46
{
}

class C46 : B46
{
    override char foo() { return 'c'; }
    char bar()
    {
        return B46.foo();
    }
}

void test46()
{
    C46 c = new C46();

    assert(c.bar() == 'a');
    printf("%c\n", c.bar());
}


/**************************************/

class Foo47
{
   static bool prop() { return false; }
   static string charprop() { return null; }
}

void test47()
{
   if (0 || Foo47.prop) { }
   if (1 && Foo47.prop) { }
   switch (Foo47.prop) { default: break; }
   foreach (char ch; Foo47.charprop) { }
}


/**************************************/

struct foo48
{
    int x, y, z;
}

void bar48() {}

void test48()
{
    foo48[] arr;
    foreach(foo48 a; arr)
    {
        bar48();
    }
}


/**************************************/

enum E49;

void test49()
{
}

/**************************************/

void test50()
{
        S50!() s;
        assert(s.i == int.sizeof);
}

struct S50()
{
        int i=f50(0).sizeof;
}

int f50(...);

/**************************************/

enum Enum51
{
        A,
        B,
        C
}

struct Struct51
{
        Enum51 e;
}

void test51()
{
        Struct51 s;
        assert(s.e == Enum51.A);
        assert(s.e == 0);
}

/**************************************/

bool foo52()
{
  int x;
  for (;;) {
    if (x == 0)
      return true;
    x = 1;
  }
  return false;
}

void test52()
{
  foo52();
}

/**************************************/

void foo53()
{
   ret:{}
   goto ret;
}

void test53()
{
}

/**************************************/

struct V54
{
    int x = 3;
}

class Foo54
{
    static int y;
    static V54 prop() { V54 val; return val; }
    static void prop(V54 val) { y = val.x * 2; }
}

void test54()
{
    (new Foo54).prop = true ? Foo54.prop : Foo54.prop;
    assert(Foo54.y == 6);
}

/**************************************/

void test55()
{
    dchar c;

    c = 'x';
    //writefln("c = '%s', c.init = '%s'", c, c.init);
    assert(c == 'x');
    assert(c.init == dchar.init);
}

/**************************************/

void writefln(string s)
{
    printf("%.*s\n", cast(int)s.length, s.ptr);
}

void test58()
{
        int label=1;
        if (0)
        {
label:
            int label2=2;
            assert(label2==2);
        }
        else
        {
            assert(label==1);
            goto label;
        }
        assert(label==1);
}

/**************************************/

void test59()
{
        if(0){
label:
                return;
        }else{
                goto label;
        }
        assert(0);
}

/**************************************/

int main(string[] argv)
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
    test40();
    test42();
    test43();
    test44();
    test45();
    test46();
    test47();
    test48();
    test49();
    test50();
    test51();
    test52();
    test53();
    test54();
    test55();
    test58();
    test59();

    printf("Success\n");
    return 0;
}
