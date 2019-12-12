// PERMUTE_ARGS: -w

extern(C) int printf(const char*, ...);

int testswitch(string h)
{
    int x;

    switch (h)
    {
        case "abc":
            printf("abc\n");
            x = 4;
            break;
        case "foo":
            printf("foo\n");
            x = 1;
            break;
        case "bar":
            printf("bar\n");
            x = 2;
            break;
        default:
            printf("default\n");
            x = 3;
            break;
    }
    return x;
}

void test1()
{
    int i;

    i = testswitch("foo");
    printf("i = %d\n", i);
    assert(i == 1);
    assert(testswitch("abc") == 4);
    assert(testswitch("bar") == 2);
    assert(testswitch("hello") == 3);
    printf("Success\n");
}

/*****************************************/

void test2()
{
    int i;

    switch (5)
    {
        case 3,4,5,6:
            i = 20;
            break;

        case 7:
        default:
            assert(0);
    }
    assert(i == 20);
}


/*****************************************/

void test3()
{
    int i;

    switch (5)
    {
        case 7:
            i = 6;
            goto default;
        default:
            i = 8;
            break;

        case 3,4,5,6:
            i = 20;
            goto default;
    }
    assert(i == 8);
}


/*****************************************/

void test4()
{   int i;

    switch (5)
    {
        case 3,4,5,6:
            i = 20;
            goto default;

        case 7:
            i = 6;
            goto default;

        default:
            i = 8;
            break;
    }
    assert(i == 8);
}


/*****************************************/

void test5()
{   int i;

    switch (5)
    {
        case 7:
            i = 6;
            goto case;
        default:
            i = 8;
            break;

        case 3,4,5,6:
            i = 20;
            break;
    }
    assert(i == 20);
}


/*****************************************/

void test6()
{
    int i;

    switch (5)
    {
        case 7:
            i = 6;
            goto case 4;
        default:
            i = 8;
            break;

        case 3,4,5,6:
            i = 20;
            break;
    }
    assert(i == 20);
}


/*****************************************/

void test7()
{
    int i;

    switch (5)
    {
        case 3,4,5,6:
            i = 20;
            break;

        case 7:
            i = 6;
            goto case 4;
        default:
            i = 8;
            break;
    }
    assert(i == 20);
}


/*****************************************/

void test8()
{
    dstring str = "xyz";
    switch (str)
    {
        case "xyz":
            printf("correct\n");
            return;

        case "abc":
            break;

        default:
            assert(0);
    }
    assert(0);
}

/*****************************************/

void test9()
{
    int i = 1;

    switch(i)
    {
        case 2:
            return;
        case 1:
            switch(i)
            {
                case 1:
                    goto case 2;
                default:
                    assert(0);
            }
        default:
            assert(0);
    }
    assert(0);
}

/*****************************************/

void test10()
{
    int id1 = 0;
    int id;
    switch (id1)
    {
        case 0: ++id; goto case;
        case 7: ++id; goto case;
        case 6: ++id; goto case;
        case 5: ++id; goto case;
        case 4: ++id; goto case;
        case 3: ++id; goto case;
        case 2: ++id; goto case;
        case 1: ++id; goto default;
        default:
            break;
    }
    assert(id == 8);
}

/*****************************************/

void test11()
{
    long foo = 4;
    switch (foo)
    {
        case 2: assert (false);
        case 3: break;
        case 4: break;
        case 5: break;
        default: assert(0);
    }
}

/*****************************************/

void test12()
{
    switch("#!")
    {
        case "#!": printf("----Found #!\n");    break;
        case "\xFF\xFE"c:                       break;
        default:
            assert(0);
    }
}

/*****************************************/

void test13()
{
    switch("#!")
    {
        case "#!": printf("----Found #!\n");    break;
        case "#\xFE"c:                          break;
        default:
            assert(0);
    }
}

/*****************************************/

void foo14(A...)(int i)
{
    switch (i)
    {
            foreach(a; A)
            {
                goto case;
            case a:
                printf("%d\n", a);
            }
            break;
        default:
            assert(0);
    }
}

void bar14(A...)(int i)
{
    switch (i)
    {
        foreach(j, a; A)
        {
            goto case;
        case A[j]:
            printf("a = %d, A[%d] = %d\n", a, j, A[j]);
        }
        break;
    default:
        assert(0);
    }
}

void test14()
{
    foo14!(1,2,3,4,5)(1);
    bar14!(1,2,3,4,5)(1);
}

/*****************************************/

const int X15;
immutable int Y15;
const int Z15;

int foo15(int i)
{
    auto y = 1;
    switch (i)
    {
        case X15:
            y += 1;
            goto case;
        case 3:
            y += 2;
            break;
        case Y15:
            y += 20;
            goto case;
        case Z15:
            y += 10;
            break;
        default:
            y += 4;
            break;
    }
    printf("y = %d\n", y);
    return y;
}

static this()
{
    X15 = 4;
    Y15 = 4;
    Z15 = 5;
}

void test15()
{
    auto i = foo15(3);
    assert(i == 3);
    i = foo15(4);
    assert(i == 4);
    i = foo15(7);
    assert(i == 5);
    i = foo15(5);
    assert(i == 11);
}

/*****************************************/

enum E16
{
    A,B,C
}

void test16()
{
    E16 e = E16.A;
    final switch (e)
    {
        case E16.A:
        case E16.B:
        case E16.C:
        {}
    }
}

/*****************************************/

void test17()
{
    int i = 2;
    switch (i)
    {
        case 1: .. case 3:
            i = 5;
            break;
        default:
            assert(0);
    }
    if (i != 5)
        assert(0);

    switch (i)
    {
        case 1: .. case 3:
            i = 4;
            break;
        case 5:
            i = 6;
            break;
        default:
            assert(0);
    }
    if (i != 6)
        assert(0);
}

/*****************************************/

int test19()
{
    enum foo{ bar }
    foo x;
    final switch(x){ case foo.bar: return 0; }
}

/*****************************************/

void test20()
{
    switch(1)
    {
        mixin("case 0:{}");
        case 1:
        case 2:
        default:
    }
}

/*****************************************/

void hang3139(int x)
{
   switch(x) {
        case -9: .. case -1:
        default:
   }
}

int wrongcode3139(int x)
{
   switch(x) {
        case -9: .. case 2: return 3;
        default:
        return 4;
   }
}

static assert(wrongcode3139(-5)==3);

// bug 3139
static assert(!is(typeof(
        (long x) { switch(x) { case long.max: .. case -long.max:
        default:} return 4; }(3)
   )));


/*****************************************/

void test7358()
{
    static void test7358a()
    {
        enum X { A = 1, B = 2 }

        auto x = X.A | X.B;

        final switch(x)
        {
        case X.A:
        case X.B:
            break;
        }
    }

    bool exception;
    try
        test7358a();
    catch (Error)
        exception = true;
    assert(exception);
}

/*****************************************/
// 9263

void test9263()
{
    enum Foo { A }

    Foo f;
    final switch (f) with(Foo)
    {
        case A:
            return;
    }
}

/*****************************************/

int bar21(int i)
{
    switch (i)
    {
//      case 1: return 11;
        case 2: return 12;
        case 3: return 13;
        case 4: return 14;
        case 5: return 15;
        case 6: return 16;
        case 7: return 17;
        case 8: return 18;
        case 9: return 19;
        case 10: return 20;
        default: break;
    }

    switch (i)
    {
        case 11: return 21;
        case 12: return 22;
        case 13: return 23;
        case 14: return 24;
        case 15: return 25;
        case 16: return 26;
        case 17: return 27;
        case 18: return 28;
        case 19: return 29;
        case 20: return 30;
        default: return 31;
    }
}

void test21()
{
//      int j = bar(12);
//      printf("j = %d\n", j);

    for (int i = 2; i < 21; i++)
    {
        int j = bar21(i);
        //printf("j = %d\n", j);
        assert(j == i + 10);
    }
}

/*****************************************/

int bar22(int i)
{
    switch (i)
    {
        case 1: return i + 1;
        case 10: return i + 2;
        case 20: return i + 3;
        case 50: return i + 4;
        case 1000: return i + 5;
        default: return 28;
    }
}

void test22()
{
    assert(bar22(1) == 2);
    assert(bar22(10) == 12);
    assert(bar22(20) == 23);
    assert(bar22(50) == 54);
    assert(bar22(1000) == 1005);
    assert(bar22(0) == 28);
    assert(bar22(5) == 28);
    assert(bar22(15) == 28);
    assert(bar22(25) == 28);
    assert(bar22(58) == 28);
    assert(bar22(2000) == 28);
}

/*****************************************/

long bar23(long i)
{
    switch (i)
    {
        case 1: return i + 1;
        case 0x10_0000_0000L: return i + 2;
        case 0x20_0070_0000L: return i + 3;
        case 0x50_0000_0000L: return i + 4;
        case 0x1000_0000_8000L: return i + 5;
        default: return 28;
    }
}

void test23()
{
    assert(bar23(1) == 2);
    assert(bar23(0x10_0000_0000L) == 0x10_0000_0000L + 2);
    assert(bar23(0x20_0070_0000L) == 0x20_0070_0000L + 3);
    assert(bar23(0x50_0000_0000L) == 0x50_0000_0000L + 4);
    assert(bar23(0x1000_0000_8000L) == 0x1000_0000_8000L + 5);
    assert(bar23(0) == 28);
    assert(bar23(58) == 28);
    assert(bar23(0x10_0000_0000L+1) == 28);
    assert(bar23(0x20_0070_0000L+5) == 28);
    assert(bar23(0x50_0000_0000L+25) == 28);
    assert(bar23(0x1000_0000_8000L+1) == 28);
}

/*****************************************/
// 14352

int transmogrify14352(int input)
{
    int output = 0;
    switch (input)
    {
        case 0, 1:
            if (input == 0)
                goto case;
            else
                output++;
            goto case;
        case 2:
            output += 5;
            goto case;
        case 3:
            output += 5;
            break;
        case 4, 5, 6:
            goto default;
        case 7:
        case 8:
            output += 20;
            break;
        default:
            return -1;
    }
    return output;
}

void test14352()
{
    assert(transmogrify14352(0) == 10);
}

/*****************************************/
// Issue 14587 - DMD 2.067.1 generating crashing binary on OSX

struct Card {
    int value;
    int suit;
}

void foo14587(Card card) {
    switch(card.value) {
    case 4: case 5: case 6: case 11:
        break;
    default:
    }
}

void test14587() {
    auto card = Card(11, 1);
    foo14587(card);
}

/*****************************************/
// Issue 15396 - static immutable not recognized as constant within switch statement

void test15396()
{
    static immutable Foo = "Foobar";
    static const Bar = "BarFoo";

    foreach (var; [ "Foobar", "BarFoo" ])
    {
        switch (var)
        {
        case Foo:
            break;
        case Bar:
            break;
        default:
            assert(0, "test15396 failed");
        }
    }
}

/*****************************************/

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
    test19();
    test20();
    test7358();
    test9263();
    test21();
    test22();
    test23();
    test14352();
    test14587();
    test15396();

    printf("Success\n");
    return 0;
}
