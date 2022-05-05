/*
RUN_OUTPUT:
---
i = 1
Writer.opShl(char[])
BinaryWriter.opShl(int)
a + 1 = 2
1 + a = 2
a + b = 3
b + a = 3
i = 64
12
534
A::opShl(int 4)
4A::opShl(char[])
 A::opShl(int 12)
12A::opShl(char[])

B::opShl_r(A)
Success
---
*/

// Test operator overloading
// Ignore deprecation warnings for D1 style operator overloading
// TRANSFORM_OUTPUT: remove_lines("Deprecation: `op")

import core.stdc.stdio;

/**************************************/

class A1
{
    int opBinary(string op)(int i) if (op == "+") { return 7 + i; }
    alias opBinaryRight = opBinary;
}

void test1()
{
    A1 a = new A1();
    int i;

    i = a + 3;
    assert(i == 10);

    i = 4 + a;
    assert(i == 11);
}

/**************************************/

class A2
{
    int opBinary(string op)(int i)      if (op == "/") { return 9 + i; }
    int opBinaryRight(string op)(int i) if (op == "/") { return 17 + i; }
}

void test2()
{
    A2 a = new A2();
    int i;

    i = a / 3;
    assert(i == 12);

    i = 4 / a;
    assert(i == 21);
}

/**************************************/

class C1
{
}

class C2
{
    int opBinary(string op)(D1 d) if (op == "+") { return 1; }
    int opBinary(string op)(D2 d) if (op == "+") { return 2; }
    int opBinary(string op)(D3 d) if (op == "+") { return 3; }
    int opBinary(string op)(D4 d) if (op == "+") { return 4; }
}

class C3
{
    int opBinaryRight(string op)(D1 d) if (op == "+") { return 5; }
    int opBinaryRight(string op)(D2 d) if (op == "+") { return 6; }
    int opBinaryRight(string op)(D3 d) if (op == "+") { return 7; }
    int opBinaryRight(string op)(D4 d) if (op == "+") { return 8; }
}

class C4
{
    int opBinary(string op)(D1 d) if (op == "+") { return 9; }
    int opBinary(string op)(D2 d) if (op == "+") { return 10; }
    int opBinary(string op)(D3 d) if (op == "+") { return 11; }
    int opBinary(string op)(D4 d) if (op == "+") { return 12; }

    int opBinaryRight(string op)(D1 d) if (op == "+") { return 13; }
    int opBinaryRight(string op)(D2 d) if (op == "+") { return 14; }
    int opBinaryRight(string op)(D3 d) if (op == "+") { return 15; }
    int opBinaryRight(string op)(D4 d) if (op == "+") { return 16; }
}

class D1
{
}

class D2
{
    int opBinary(string op)(C1 d) if (op == "+") { return 17; }
    int opBinary(string op)(C2 d) if (op == "+") { return 18; }
    int opBinary(string op)(C3 d) if (op == "+") { return 19; }
    int opBinary(string op)(C4 d) if (op == "+") { return 20; }
}

class D3
{
    int opBinaryRight(string op)(C1 d) if (op == "+") { return 21; }
    int opBinaryRight(string op)(C2 d) if (op == "+") { return 22; }
    int opBinaryRight(string op)(C3 d) if (op == "+") { return 23; }
    int opBinaryRight(string op)(C4 d) if (op == "+") { return 24; }
}

class D4
{
    int opBinary(string op)(C1 d) if (op == "+") { return 25; }
    int opBinary(string op)(C2 d) if (op == "+") { return 26; }
    int opBinary(string op)(C3 d) if (op == "+") { return 27; }
    int opBinary(string op)(C4 d) if (op == "+") { return 28; }

    int opBinaryRight(string op)(C1 d) if (op == "+") { return 29; }
    int opBinaryRight(string op)(C2 d) if (op == "+") { return 30; }
    int opBinaryRight(string op)(C3 d) if (op == "+") { return 31; }
    int opBinaryRight(string op)(C4 d) if (op == "+") { return 32; }
}



void test3()
{
    C1 c1 = new C1();
    C2 c2 = new C2();
    C3 c3 = new C3();
    C4 c4 = new C4();
    D1 d1 = new D1();
    D2 d2 = new D2();
    D3 d3 = new D3();
    D4 d4 = new D4();

    int i;

    //i = c1 + d1;    assert(i == );
    //i = c1 + d2;    assert(i == );
    i = c1 + d3;    assert(i == 21);
    i = c1 + d4;    assert(i == 29);

    i = c2 + d1;    assert(i == 1);
    i = c2 + d2;    assert(i == 2);
    i = c2 + d3;    assert(i == 22);
    i = c2 + d4;    assert(i == 30);

    //i = c3 + d1;    assert(i == );
    //i = c3 + d2;    assert(i == );
    i = c3 + d3;    assert(i == 23);
    i = c3 + d4;    assert(i == 31);

    i = c4 + d1;    assert(i == 9);
    i = c4 + d2;    assert(i == 10);
    i = c4 + d3;    assert(i == 24);
    i = c4 + d4;    assert(i == 32);

    //i = d1 + c1;    assert(i == );
    //i = d1 + c2;    assert(i == );
    i = d1 + c3;    assert(i == 5);
    i = d1 + c4;    assert(i == 13);

    i = d2 + c1;    assert(i == 17);
    i = d2 + c2;    assert(i == 18);
    i = d2 + c3;    assert(i == 6);
    i = d2 + c4;    assert(i == 14);

    //i = d3 + c1;    assert(i == );
    //i = d3 + c2;    assert(i == );
    i = d3 + c3;    assert(i == 7);
    i = d3 + c4;    assert(i == 15);

    i = d4 + c1;    assert(i == 25);
    i = d4 + c2;    assert(i == 26);
    i = d4 + c3;    assert(i == 8);
    i = d4 + c4;    assert(i == 16);
}

/**************************************/

struct Foo4
{
    int a;
    int opCmp(Foo4 b)
    {
        return a < b.a;
    }
}

void test4()
{
   Foo4 a;
   Foo4 b;

   assert(a <= b);
}

/**************************************/

class A5
{
    int opUnary(string op)() if (op == "-")  { return 10; }
    int opUnary(string op)() if (op == "~")  { return 11; }
    int opUnary(string op)() if (op == "++") { return 12; }
    int opUnary(string op)() if (op == "--") { return 13; }


    int opBinary(string op)(int j)      if (op == "+")   { return 14; }
    int opBinary(string op)(int j)      if (op == "-")   { return 15; }
    int opBinaryRight(string op)(int j) if (op == "-")   { return 16; }
    int opBinary(string op)(int j)      if (op == "*")   { return 17; }
    int opBinary(string op)(int j)      if (op == "/")   { return 18; }
    int opBinaryRight(string op)(int j) if (op == "/")   { return 19; }
    int opBinary(string op)(int j)      if (op == "%")   { return 20; }
    int opBinaryRight(string op)(int j) if (op == "%")   { return 21; }
    int opBinary(string op)(int j)      if (op == "&")   { return 22; }
    int opBinary(string op)(int j)      if (op == "|")   { return 23; }
    int opBinary(string op)(int j)      if (op == "^")   { return 24; }
    int opBinary(string op)(int j)      if (op == "<<")  { return 25; }
    int opBinaryRight(string op)(int j) if (op == "<<")  { return 26; }
    int opBinary(string op)(int j)      if (op == ">>")  { return 27; }
    int opBinaryRight(string op)(int j) if (op == ">>")  { return 28; }
    int opBinary(string op)(int j)      if (op == ">>>") { return 29; }
    int opBinaryRight(string op)(int j) if (op == ">>>") { return 30; }
    int opBinary(string op)(int j)      if (op == "~")   { return 31; }
    int opBinaryRight(string op)(int j) if (op == "~")   { return 32; }
    int opEquals(int j)  { return 33; }
    int opCmp(int j)     { return 34; }
    int opOpAssign(string op)(int j) if (op == "+")   { return 35; }
    int opOpAssign(string op)(int j) if (op == "-")   { return 36; }
    int opOpAssign(string op)(int j) if (op == "*")   { return 37; }
    int opOpAssign(string op)(int j) if (op == "/")   { return 38; }
    int opOpAssign(string op)(int j) if (op == "%")   { return 39; }
    int opOpAssign(string op)(int j) if (op == "&")   { return 40; }
    int opOpAssign(string op)(int j) if (op == "|")   { return 41; }
    int opOpAssign(string op)(int j) if (op == "^")   { return 42; }
    int opOpAssign(string op)(int j) if (op == "<<")  { return 43; }
    int opOpAssign(string op)(int j) if (op == ">>")  { return 44; }
    int opOpAssign(string op)(int j) if (op == ">>>") { return 45; }
    int opOpAssign(string op)(int j) if (op == "~")   { return 46; }
}

void test5()
{
    A5 a = new A5();
    int i;

    i = -a;
    assert(i == 10);

    i = ~a;
    assert(i == 11);

    i = ++a;
    assert(i == 12);

    i = --a;
    assert(i == 13);

    i = a + 1;
    assert(i == 14);

    i = a - 1;
    assert(i == 15);

    i = 1 - a;
    assert(i == 16);

    i = a * 1;
    assert(i == 17);

    i = a / 1;
    assert(i == 18);

    i = 1 / a;
    assert(i == 19);

    i = a % 1;
    assert(i == 20);

    i = 1 % a;
    assert(i == 21);

    i = a & 1;
    assert(i == 22);

    i = a | 1;
    assert(i == 23);

    i = a ^ 1;
    assert(i == 24);

    i = a << 1;
    assert(i == 25);

    i = 1 << a;
    assert(i == 26);

    i = a >> 1;
    assert(i == 27);

    i = 1 >> a;
    assert(i == 28);

    i = a >>> 1;
    assert(i == 29);

    i = 1 >>> a;
    assert(i == 30);

    i = a ~ 1;
    assert(i == 31);

    i = 1 ~ a;
    assert(i == 32);

    i = a == 1;
    assert(i == 33);
    i = 1 == a;
    assert(i == 33);
    i = a != 1;
    assert(i == 0);
    i = 1 != a;
    assert(i == 0);

    i = a < 1;
    assert(i == 0);
    i = a <= 1;
    assert(i == 0);
    i = a > 1;
    assert(i == 1);
    i = a >= 1;
    assert(i == 1);

    i = 1 < a;
printf("i = %d\n", i);
    assert(i == 1);
    i = 1 <= a;
    assert(i == 1);
    i = 1 > a;
    assert(i == 0);
    i = 1 >= a;
    assert(i == 0);

    i = (a += 1);
    assert(i == 35);

    i = (a -= 1);
    assert(i == 36);

    i = (a *= 1);
    assert(i == 37);

    i = (a /= 1);
    assert(i == 38);

    i = (a %= 1);
    assert(i == 39);

    i = (a &= 1);
    assert(i == 40);

    i = (a |= 1);
    assert(i == 41);

    i = (a ^= 1);
    assert(i == 42);

    i = (a <<= 1);
    assert(i == 43);

    i = (a >>= 1);
    assert(i == 44);

    i = (a >>>= 1);
    assert(i == 45);

    i = (a ~= 1);
    assert(i == 46);
}

/*********************************/

struct A6
{
    int opUnary(string op)() if (op == "-")  { return 10; }
    int opUnary(string op)() if (op == "~")  { return 11; }
    int opUnary(string op)() if (op == "++") { return 12; }
    int opUnary(string op)() if (op == "--") { return 13; }

    int opBinary(string op)(int j)      if (op == "+")   { return 14; }
    int opBinary(string op)(int j)      if (op == "-")   { return 15; }
    int opBinaryRight(string op)(int j) if (op == "-")   { return 16; }
    int opBinary(string op)(int j)      if (op == "*")   { return 17; }
    int opBinary(string op)(int j)      if (op == "/")   { return 18; }
    int opBinaryRight(string op)(int j) if (op == "/")   { return 19; }
    int opBinary(string op)(int j)      if (op == "%")   { return 20; }
    int opBinaryRight(string op)(int j) if (op == "%")   { return 21; }
    int opBinary(string op)(int j)      if (op == "&")   { return 22; }
    int opBinary(string op)(int j)      if (op == "|")   { return 23; }
    int opBinary(string op)(int j)      if (op == "^")   { return 24; }
    int opBinary(string op)(int j)      if (op == "<<")  { return 25; }
    int opBinaryRight(string op)(int j) if (op == "<<")  { return 26; }
    int opBinary(string op)(int j)      if (op == ">>")  { return 27; }
    int opBinaryRight(string op)(int j) if (op == ">>")  { return 28; }
    int opBinary(string op)(int j)      if (op == ">>>") { return 29; }
    int opBinaryRight(string op)(int j) if (op == ">>>") { return 30; }
    int opBinary(string op)(int j)      if (op == "~")   { return 31; }
    int opBinaryRight(string op)(int j) if (op == "~")   { return 32; }
    int opEquals(int j)  { return 33; }
    const bool opEquals(const ref A6)      { return false; }
    int opCmp(int j)     { return 34; }
    int opOpAssign(string op)(int j) if (op == "+")   { return 35; }
    int opOpAssign(string op)(int j) if (op == "-")   { return 36; }
    int opOpAssign(string op)(int j) if (op == "*")   { return 37; }
    int opOpAssign(string op)(int j) if (op == "/")   { return 38; }
    int opOpAssign(string op)(int j) if (op == "%")   { return 39; }
    int opOpAssign(string op)(int j) if (op == "&")   { return 40; }
    int opOpAssign(string op)(int j) if (op == "|")   { return 41; }
    int opOpAssign(string op)(int j) if (op == "^")   { return 42; }
    int opOpAssign(string op)(int j) if (op == "<<")  { return 43; }
    int opOpAssign(string op)(int j) if (op == ">>")  { return 44; }
    int opOpAssign(string op)(int j) if (op == ">>>") { return 45; }
    int opOpAssign(string op)(int j) if (op == "~")   { return 46; }
}

void test6()
{
    A6 a;
    int i;

    i = -a;
    assert(i == 10);

    i = ~a;
    assert(i == 11);

    i = ++a;
    assert(i == 12);

    i = --a;
    assert(i == 13);

    i = a + 1;
    assert(i == 14);

    i = a - 1;
    assert(i == 15);

    i = 1 - a;
    assert(i == 16);

    i = a * 1;
    assert(i == 17);

    i = a / 1;
    assert(i == 18);

    i = 1 / a;
    assert(i == 19);

    i = a % 1;
    assert(i == 20);

    i = 1 % a;
    assert(i == 21);

    i = a & 1;
    assert(i == 22);

    i = a | 1;
    assert(i == 23);

    i = a ^ 1;
    assert(i == 24);

    i = a << 1;
    assert(i == 25);

    i = 1 << a;
    assert(i == 26);

    i = a >> 1;
    assert(i == 27);

    i = 1 >> a;
    assert(i == 28);

    i = a >>> 1;
    assert(i == 29);

    i = 1 >>> a;
    assert(i == 30);

    i = a ~ 1;
    assert(i == 31);

    i = 1 ~ a;
    assert(i == 32);

    i = a == 1;
    assert(i == 33);
    i = 1 == a;
    assert(i == 33);
    i = a != 1;
    assert(i == 0);
    i = 1 != a;
    assert(i == 0);

    i = a < 1;
    assert(i == 0);
    i = a <= 1;
    assert(i == 0);
    i = a > 1;
    assert(i == 1);
    i = a >= 1;
    assert(i == 1);

    i = 1 < a;
    assert(i == 1);
    i = 1 <= a;
    assert(i == 1);
    i = 1 > a;
    assert(i == 0);
    i = 1 >= a;
    assert(i == 0);

    i = (a += 1);
    assert(i == 35);

    i = (a -= 1);
    assert(i == 36);

    i = (a *= 1);
    assert(i == 37);

    i = (a /= 1);
    assert(i == 38);

    i = (a %= 1);
    assert(i == 39);

    i = (a &= 1);
    assert(i == 40);

    i = (a |= 1);
    assert(i == 41);

    i = (a ^= 1);
    assert(i == 42);

    i = (a <<= 1);
    assert(i == 43);

    i = (a >>= 1);
    assert(i == 44);

    i = (a >>>= 1);
    assert(i == 45);

    i = (a ~= 1);
    assert(i == 46);
}


/**************************************/

struct Foo7
{
    int opSlice() { return 7; }
    int opSlice(int i, int j) { return i * (j + 1); }
}

void test7()
{
    Foo7 f;
    int i;

    i = f[];
    assert(i == 7);
    i = f[3..4];
    assert(i == 15);
}

/**************************************/

interface IWriter
{
        int opBinary(string op)(string i) if (op == "<<");
        int opBinary(string op)(int i) if (op == "<<");
}

class Writer : IWriter
{
    int opBinary(string op)(string i) if (op == "<<")
    {
        printf("Writer.opShl(char[])\n");
        return 1;
    }

    int opBinary(string op)(int i) if (op == "<<")
    {
        printf("Writer.opShl(int)\n");
        return 2;
    }
}

class BinaryWriter : Writer
{
    int opBinary(string op)(string i) if (op == "<<")
    {
        printf("Writer.opShl(char[])\n");
        return 1;
    }

    int opBinary(string op)(int i) if (op == "<<")
    {
        printf("BinaryWriter.opShl(int)\n");
        return 3;
    }
}

void test8()
{
    BinaryWriter bw = new BinaryWriter();
    int i;

    i = bw << "test";
    assert(i == 1);
    i = bw << 1;
    assert(i == 3);
}

/**************************************/

struct A9
{
    int opCast() { return 28; }
}

void test9()
{
    A9 a;

    long i = cast(long)a;
    assert(i == 28);
}

/**************************************/

class A10
{
    int opBinary(string op)(int i) if (op == "+") { return i + 1; }
    alias opBinaryRight = opBinary;
}

class B10
{
    int opBinaryRight(string op)(A10 a) if (op == "+") { return 3; }
    alias opBinary = opBinaryRight;
}

void test10()
{
    int i;
    A10 a = new A10();

    i = a + 1;
    printf("a + 1 = %d\n", i);
    assert(i == 2);

    i = 1 + a;
    printf("1 + a = %d\n", i);
    assert(i == 2);

    B10 b = new B10();
    i = a + b;
    printf("a + b = %d\n", i);
    assert(i == 3);

    i = b + a;
    printf("b + a = %d\n", i);
    assert(i == 3);

//    i = b + 3;
}

/**************************************/

class A11
{
    int opIndex(int i) { return i; }
    int opIndexAssign(int value, int i) { return value * 10 + i; }
}

void test11()
{
    int i;
    A11 a = new A11();

    i = a[5];
    assert(i == 5);
    i = (a[4] = 6);
    printf("i = %d\n", i);
    assert(i == 64);
}

/**************************************/

class A12
{
    int opIndex(int i1, int i2) { return i1 * 10 + i2; }
    int opIndexAssign(int value, int i1, int i2) { return value * 100 + i1 * 10 + i2; }
}


void test12()
{
   A12 a = new A12();
   int i;

   printf("%d\n", a[1, 2]);
   assert(a[1, 2] == 12);

   i = (a[3, 4] = 5);
   printf("%d\n", i);
   assert(i == 534);
}

/**************************************/

class A13
{
 A13 opBinary(string op)(int x) if (op == "<<")
 {
    printf("A::opShl(int %d)\n", x);
    printf("%d",x);
    return this;
 }
 A13 opBinary(string op)(string x) if (op == "<<")
 {
    printf("A::opShl(char[])\n");
    printf("%.*s", cast(int)x.length, x.ptr);
    return this;
 }
}

class B13
{
 A13 opBinaryRight(string op)(A13 a) if (op == "<<")
 {
    printf("B::opShl_r(A)\n");
    return a;
 }
}

void test13()
{
    A13 a = new A13();
    a << 4 << " " << 12 << "\n";

    B13 b = new B13();
    a << b;
}


/**************************************/

class Foo14
{   int a;

    int opBinary(string op)(int x) if (op == "in")
    {
        return a + x;
    }
}

void test14()
{
    auto f = new Foo14;
    f.a = 3;
    auto i = f in 7;
    assert(i == 10);
}

/**************************************/

// https://issues.dlang.org/show_bug.cgi?id=3983

struct Fug
{
    bool opEquals(ref const Fug y) const {
        return false;
    }
}

struct Fig
{
   Fug f;
   bool opEquals(Tdummy=void)(ref const Fig y) const {
      return false;
   }

   bool opEquals(T: int)(T y) const {
      return false;
   }
}

void test15()
{
  Fig fx, fy;
  if (fx==2) {}
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=4953

struct S4953a
{
    short _x;
    bool opBinaryRight(string op)(short x) if (op == "in")
    {
        return x == _x;
    }
}
void test4953a()
{
    S4953a s;
    5 in s;
}

struct S4953b
{
    void opBinary(string op)(short x)
    {}
}
void test4953b()
{
    S4953b s;
    s + 5;
}


struct S4953c
{
    void funOpAssign(float[1u] data) { }
    void opOpAssign(string op)(float[1u] data) if(op=="<<") { }
}
void test4953c()
{
    // dmd v2.054, v2.055
    S4953c s;
    float[1u] a = [1.0f]; // OK: Implicit cast from float[] compiles
    s.funOpAssign([1.0f]); // OK: Implicit cast from float[] compiles
    s <<= [1.0f]; // Issue: Implicit cast from float[] does not compile
    s <<= cast(float[1u])[1.0f]; // OK: Explicit cast from float[] compiles
}

void f4953d1  (dstring d) { dstring e = d; }
void f4953d2()(dstring d) { dstring e = d; }
void f4953d3  ( byte[] b) { byte[] c = b; }
void f4953d4()( byte[] b) { byte[] c = b; }
void test4953d()
{
    f4953d1("abc");  // OK
    f4953d2("abc");  // OK
    f4953d3([1,2,3]); // OK
    f4953d4([1,2,3]);
    // Error: template test2.f() does not match any function template declaration
    // Error: template test2.f() cannot deduce template function from argument types !()(int[])
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=4993

// reduced from the bug report
struct Bar4993
{
    void opIndexAssign(int value, size_t index) {}
}
@property auto bar4993()
{
    return Bar4993();
}
void test4993()
{
    bar4993[3] = 42;
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=8133

void test8133()
{
    struct S
    {
        int opCall() { return 1; }
    }
    struct A
    {
        S s;
        alias s this;
    }
    A f = A();
    assert(f() == 1);
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=8522

struct Point8522
{
    bool opEquals(R)(R rhs) { return true; }
    bool opEquals(R)(R rhs) const { return true; }
}

void test8522()
{
    Point8522 mp;
    const Point8522 cp;
    assert(mp == mp);
    assert(mp == cp);
    assert(cp == mp);   // doesn't work
    assert(cp == cp);   // doesn't work
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=12778

struct Vec12778X
{
    Vec12778X opBinary(string op)(Vec12778X b) const
    if (op == "+")
    {
        mixin("return Vec12778X(this.x " ~ op ~ " b.x, this.y " ~ op ~ " b.y);");
    }
    alias opBinaryRight = opBinary;

    float x = 0, y = 0;
}

void test12778()
{
    struct S
    {
        void test1()
        {
            Vec12778X vx = vx1 + vx2;   // ok
        }

        void test2() const
        {
            Vec12778X vx = vx1 + vx2;   // ok <- error
        }

        Vec12778X vx1, vx2;
    }
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=14343

struct S14343a
{
    int i;
    immutable(Object) o;

    S14343a opUnary(string op)() { return this; }
    void opAssign(S14343a other) {}
}

struct S14343b
{
    int i;
    immutable(Object) o;

    void opOpAssign(string op)(int j) if (op == "+") { i += j; }
    S14343b opUnary(string op)() if (op == "++") { ++i; return this; }
    void opAssign(S14343b other) {}
}

void test14343()
{
    {
        S14343a s, t;

        t = s;  // OK
        ++s;    // OK
        s++;    // OK <- Error: cannot modify struct s S with immutable members
    }
    {
        S14343b s;
        ++s;
        assert(s.i == 1);
        s++;
        assert(s.i == 2);
    }
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=14344

struct S14344
{
    S14344 opBinary(string op)(S14344 v)
    {
        static assert(0);
    }
    S14344 opAssign()(S14344 v)
    {
        static assert(0);
    }
}

struct S14344Mix
{
    S14344 s;
    alias s this;
}

class C14344
{
    S14344Mix height() { return S14344Mix(); }

    void update()
    {
        S14344 height = this.height;
    }
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=1547
struct A
{
    int b;
    static A opCall(int k)
    {
        assert(0);
    }
    this(int) {}
}

void fun(A k = 2) {}

void test1547()
{
    fun();
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=20475
struct S20475
{
    string[2] x;
}

void test20475()
{
    auto s = S20475(["abc", "bcd"]);
    auto t = S20475(["abc", ""]);
    string u = "abcd";
    t.x[1] = u[1..$];
    assert(s == t);
}

/**************************************/
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
    test1547();
    test4953a();
    test4953b();
    test4953c();
    test4953d();
    test4993();
    test8133();
    test8522();
    test20475();

    printf("Success\n");
    return 0;
}
