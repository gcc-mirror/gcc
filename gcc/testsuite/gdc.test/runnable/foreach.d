/*
RUN_OUTPUT:
---
u = 17
u = 1
u = 1
u = 1
u = 1
a[0] = 21
a[1] = 22
a[2] = 23
a[] = 21
a[] = 22
a[] = 23
a = 63, b = 47, c = 83
a = 63, b = 48, c = 83
Success
---
*/

import core.stdc.stdio;

/**************************************************/

void test1()
{
    int i;

    foreach (char c; "abcd")
    {
        switch (i++)
        {   case 0:     assert(c == 'a');   break;
            case 1:     assert(c == 'b');   break;
            case 2:     assert(c == 'c');   break;
            case 3:     assert(c == 'd');   break;
            default:    assert(0);
        }
    }

    i = 0;
    foreach (wchar c; "asdf")
    {
        switch (i++)
        {   case 0:     assert(c == 'a');   break;
            case 1:     assert(c == 's');   break;
            case 2:     assert(c == 'd');   break;
            case 3:     assert(c == 'f');   break;
            default:    assert(0);
        }
    }

    i = 0;
    foreach (dchar c; "bncd")
    {
        switch (i++)
        {   case 0:     assert(c == 'b');   break;
            case 1:     assert(c == 'n');   break;
            case 2:     assert(c == 'c');   break;
            case 3:     assert(c == 'd');   break;
            default:    assert(0);
        }
    }
}

/**************************************************/

void test2()
{
    int i;

    uint[5] a;
    a[0] = 16;
    a[1] = 1;
    a[2] = 5;
    a[3] = 8;
    a[4] = 3;

    foreach (uint u; a)
    {
        switch (i++)
        {   case 0:     assert(u == 16);    break;
            case 1:     assert(u == 1);     break;
            case 2:     assert(u == 5);     break;
            case 3:     assert(u == 8);     break;
            case 4:     assert(u == 3);     break;
            default:    assert(0);
        }
    }

    uint[] b = a;

    i = 0;
    foreach (uint u; b)
    {
        switch (i++)
        {   case 0:     assert(u == 16);    break;
            case 1:     assert(u == 1);     break;
            case 2:     assert(u == 5);     break;
            case 3:     assert(u == 8);     break;
            case 4:     assert(u == 3);     break;
            default:    assert(0);
        }
    }

    test2_x(a);
}

void test2_x(uint[5] a)
{
    int i;

    foreach (uint u; a)
    {
        switch (i++)
        {   case 0:     assert(u == 16);    break;
            case 1:     assert(u == 1);     break;
            case 2:     assert(u == 5);     break;
            case 3:     assert(u == 8);     break;
            case 4:     assert(u == 3);     break;
            default:    assert(0);
        }
    }
}

/**************************************************/

void test3()
{
    int i;

    uint[5] a;
    a[0] = 16;

    foreach (ref uint u; a)
    {
        i += u;
        u++;
    }
    assert(i == 16);
    assert(a[0] == 17);
    assert(a[4] == 1);

    foreach (uint u; a)
    {
        printf("u = %d\n", u);
        //u++;
    }
    assert(a[0] == 17);
    assert(a[4] == 1);
}

/**************************************************/

enum E4 { m }

struct X4 {
    char [] b;
    E4 a;
}

void test4()
{
    X4 [] x;
    foreach (X4 w; x) {}
}


/**************************************************/

class Thing5
{}

class Things5
{
public:
  int opApply(int delegate(ref Thing5 thing) dg)
  {
    Thing5 thing = new Thing5();

    return dg(thing);
  }
}

void foo5(Things5 things)
{
    foreach(Thing5 t; things)
    {
    }
}

void test5()
{
}


/**************************************************/

void test6()
{
    static long[3] a = [21,22,23];
    long[3] b;
    int sum;

    foreach (int i, ref long v; a)
    {
        printf("a[%d] = %lld\n", i, v);
        b[i] = v;
    }

    for (uint i = 0; i < 3; i++)
    {
        assert(b[i] == 21 + i);
    }

    foreach (ref long v; a)
    {
        printf("a[] = %lld\n", v);
        sum += v;
    }
    assert(sum == 21 + 22 + 23);
}

/**************************************************/

void test7()
{
    uint[string] a;

    a["foo"] = 3;
    a["bar"] = 4;
    bool sawBar, sawFoo;
    foreach (string s, uint v; a)
    {
        if (s == "bar")
        {
            assert(v == 4);
            assert(!sawBar);
            sawBar = true;
        }
        else if (s == "foo")
        {
            assert(v == 3);
            assert(!sawFoo);
            sawFoo = true;
        }
        else
            assert(0);
    }
    assert(sawBar);
    assert(sawFoo);
}


/**************************************************/

class Foo8
{
    int x, y, z;

    int opApply(int delegate(ref int a, ref int b, ref int c) dg)
    {
        int result = dg(x, y, z);
        return 0;
    }
}

void test8()
{
    Foo8 f = new Foo8();
    f.x = 63;
    f.y = 47;
    f.z = 83;
    foreach (int a, ref int b, int c; f)
    {
        printf("a = %d, b = %d, c = %d\n", a, b, c);
        assert(a == 63);
        assert(b == 47);
        assert(c == 83);
        a++;
        b++;
        c++;
    }
    foreach (int a, ref int b, int c; f)
    {
        printf("a = %d, b = %d, c = %d\n", a, b, c);
        assert(a == 63);
        assert(b == 48);
        assert(c == 83);
        a++;
        b++;
        c++;
    }
}

/**************************************************/

struct S
{
    int opApply(int delegate(ref int a)) { return 0; }
    int opApplyReverse(int delegate(ref int a)) { return 0; }
    int dg(int delegate(ref int a)) { return 0; }
}

void test9()
{
    S s;
    foreach(a; s) {}
    foreach_reverse(a; s) {}
    foreach(a; &s.dg) {}
}

/**************************************************/

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

    printf("Success\n");
    return 0;
}
