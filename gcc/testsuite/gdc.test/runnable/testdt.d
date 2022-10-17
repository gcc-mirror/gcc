// PERMUTE_ARGS:

/******************************************/

static int[100][100] bigarray;

void test1()
{
  for (int i = 0; i < 100; i += 1)
  {
    for (int j = 0; j < 100; j += 1)
    {
      //printf("Array %i %i\n", i, j);
      bigarray[i][j] = 0;
    }
  }
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=10629

class Foo10629 {}

struct Bar10629
{
    void[__traits(classInstanceSize, Foo10629)] x;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11233

struct S11233
{
    uint[0x100000] arr;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=11672

void test11672()
{
    struct V { float f; }
    struct S
    {
        V[3] v = V(1);
    }

    S s;
    assert(s.v == [V(1), V(1), V(1)]); /* was [V(1), V(nan), V(nan)] */
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12509

struct A12509
{
    int member;
}
struct B12509
{
    A12509[0x10000] array;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13505

class C13505 { void[10] x; }
struct S13505 { void[10] x; }

void test13505()
{
    auto c = new C13505();
    auto s = S13505();
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14699

struct S14699a { ubyte[0][10] values; }
struct S14699b { S14699a tbl; }
// +/

ubyte[0][1] sa14699;

void test14699()
{
    //auto p = &sa14699;  // Cannot work in Win32 (OMF)
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=14805

struct S14805
{
    ushort one;
}
auto a14805 = new S14805[513*513];

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=15664

struct Data15664A
{
    int[2] a;
    int[2][2] b;
    int c;
}

             Data15664A d15664a1 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK
       const Data15664A d15664a2 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK <- NG
shared       Data15664A d15664a3 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK <- NG
shared const Data15664A d15664a4 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK <- NG
   immutable Data15664A d15664a5 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK <- NG

struct Data15664B
{
    int[2] a;
    const int[2][2] b;
    int c;
}

             Data15664B d15664b1 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK
       const Data15664B d15664b2 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK
shared       Data15664B d15664b3 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK <- NG
shared const Data15664B d15664b4 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK <- NG
   immutable Data15664B d15664b5 = {[1, 2], [[3, 4], [5, 6]], 7};   // OK <- NG

void test15664()
{
    assert(d15664a1.a == [1, 2]);
    assert(d15664a2.a == [1, 2]);
    assert(d15664a3.a == cast(shared)[1, 2]);
    assert(d15664a4.a == cast(shared)[1, 2]);
    assert(d15664a5.a == [1, 2]);
    assert(d15664a1.b == [[3, 4], [5, 6]]);
    assert(d15664a2.b == [[3, 4], [5, 6]]);
    assert(d15664a3.b == cast(shared)[[3, 4], [5, 6]]);
    assert(d15664a4.b == cast(shared)[[3, 4], [5, 6]]);
    assert(d15664a5.b == [[3, 4], [5, 6]]);
    assert(d15664a1.c == 7);    // OK
    assert(d15664a2.c == 7);    // OK <- BG
    assert(d15664a3.c == 7);    // OK <- NG
    assert(d15664a4.c == 7);    // OK <- NG
    assert(d15664a5.c == 7);    // OK <- NG

    assert(d15664b1.a == [1, 2]);
    assert(d15664b2.a == [1, 2]);
    assert(d15664b3.a == cast(shared)[1, 2]);
    assert(d15664b4.a == cast(shared)[1, 2]);
    assert(d15664b5.a == [1, 2]);
    assert(d15664b1.b == [[3, 4], [5, 6]]);
    assert(d15664b2.b == [[3, 4], [5, 6]]);
    assert(d15664b3.b == cast(shared)[[3, 4], [5, 6]]);
    assert(d15664b4.b == cast(shared)[[3, 4], [5, 6]]);
    assert(d15664b5.b == [[3, 4], [5, 6]]);
    assert(d15664b1.c == 7);    // OK
    assert(d15664b2.c == 7);    // OK
    assert(d15664b3.c == 7);    // OK <- NG
    assert(d15664b4.c == 7);    // OK <- NG
    assert(d15664b5.c == 7);    // OK <- NG
}

/******************************************/

int main()
{
    test1();
    test11672();
    test15664();

    return 0;
}
