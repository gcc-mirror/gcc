// NOTE: this is a dup of runnable/tls.d strictly to test the same code compiled
// separately rather than together like the original is.

// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/tlsa.d
// PERMUTE_ARGS:

import core.stdc.stdio;
import imports.tlsa;

int x = 3;

void bar()
{
    int* px = &x;
    assert(x == 3);
    x++;
    printf("x = %d\n", x);
    px = &x;
    printf("px = %p\n", px);
    assert(*px == 4);
    (*px)++;
    assert(x == 5);
}


void test1()
{
    bar();
    printf("%d\n", x);
    printf("%d\n", foo!()());
}

/************************************/

long fooa;
long foob;
int bara = 0x12345678;
int barb = 0x9ABCDEFF;

void test2()
{
    fooa++;
    foob--;
    bara++;
    barb++;
    printf("%lld %lld %x %x\n", fooa, foob, bara, barb);
    assert(fooa == 1);
    assert(foob == -1);
    assert(bara == 0x12345679);
    assert(barb == 0x9ABCDF00);
}

/************************************/

int abc3(T)(T t)
{
    static T qqq;
    static T rrr;
    static T sss = 8;
    static T ttt = 9;
    printf("qqq = %d, rrr = %d, sss = %d, ttt = %d\n", qqq, rrr, sss, ttt);
    assert(sss == 8);
    assert(ttt == 9);
    rrr += 7;
    return t + ++qqq + rrr;
}

void test3()
{
    auto i = abc3(3);
    printf("i = x%x\n", i);
    assert(i == 11);
    i = abc3(4);
    printf("i = x%x\n", i);
    assert(i == 20);
}

/************************************/

void test4()
{

    auto i = bar4();
    printf("i = x%x\n", i);
    assert(i == 0x23);
    i = abc4(4);
    printf("i = x%x\n", i);
    assert(i == 0x31);
}

/************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();

    printf("Success\n");
    return 0;
}

