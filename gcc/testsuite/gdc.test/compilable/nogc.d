// REQUIRED_ARGS: -o-

/***************** Covariance ******************/

class C1
{
    void foo() @nogc;
    void bar();
}

class D1 : C1
{
    override void foo();        // no error
    override void bar() @nogc;  // no error
}

/******************************************/
// __traits(compiles)

static assert(__traits(compiles, new Object()));

void foo_compiles() {}

@nogc void test_compiles()
{
    auto fp = &foo_compiles;
    static assert(!__traits(compiles, foo_compiles()));
    static assert(!__traits(compiles, fp()));
    static assert(!__traits(compiles, (*fp)()));

    static assert(!__traits(compiles, [1,2,3]));
    static assert(!__traits(compiles, [1:1, 2:2]));

    struct Struct {}
    static assert(!__traits(compiles, new int));
    static assert(!__traits(compiles, new Struct()));
    static assert(!__traits(compiles, new Object()));

    int* p;
    static assert(!__traits(compiles, delete p));

    int[int] aa;
    static assert(!__traits(compiles, aa[0]));

    int[] a;
    static assert(!__traits(compiles, a.length = 1));
    static assert(!__traits(compiles, a.length += 1));
    static assert(!__traits(compiles, a.length -= 1));

    static assert(!__traits(compiles, a ~= 1));
    static assert(!__traits(compiles, a ~ a));
}

/******************************************/
// 12630

void test12630() @nogc
{
    // All of these declarations should cause no errors.

    static const ex1 = new Exception("invalid");
  //enum         ex2 = new Exception("invalid");

    static const arr1 = [[1,2], [3, 4]];
    enum         arr2 = [[1,2], [3, 4]];

  //static const aa1 = [1:1, 2:2];
    enum         aa2 = [1:1, 2:2];

  //static const v1 = aa1[1];
    enum         v2 = aa2[1];

    Object o;
    //static const del1 = (delete o).sizeof;
    //enum         del2 = (delete o).sizeof;

    int[] a;
    static const len1 = (a.length = 1).sizeof;
    enum         len2 = (a.length = 1).sizeof;

    static const cata1 = (a ~= 1).sizeof;
    enum         cata2 = (a ~= 1).sizeof;

    static const cat1 = (a ~ a).sizeof;
    enum         cat2 = (a ~ a).sizeof;
}

/******************************************/
// 12642

static if (is(__vector(ulong[2])))
{
    import core.simd;

    ulong2 test12642() @nogc
    {
        return [0, 0];
    }
}

/******************************************/
// 13550

auto foo13550() @nogc
{
    static int[] bar()
    {
        return new int[2];
    }
    return &bar;
}
