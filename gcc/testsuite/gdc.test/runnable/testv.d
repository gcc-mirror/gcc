
extern(C) int printf(const char*, ...);

/*********************************************************/

int sum(int[] xx ...)
{
    int s;

    foreach (int x; xx)
        s += x;
    return s;
}

void test1()
{
    static int[3] a = [5, 8, 10];
    int[] b = a;
    int i;

    i = sum();
    assert(i == 0);
    i = sum(10);
    assert(i == 10);
    i = sum(10, 20);
    assert(i == 30);
    i = sum(11, 22, 34);
    assert(i == 67);
    i = sum(a);
    assert(i == 23);
    i = sum(b);
    assert(i == 23);
    printf("%d\n", sum());
}

/*********************************************************/

int sum2(int[3] xx ...)
{
    int s;

    foreach (int x; xx)
        s += x;
    return s;
}

void test2()
{
    static int[3] a = [5, 8, 10];
    int i;

    i = sum2(11, 22, 34);
    assert(i == 67);
    i = sum2(a);
    assert(i == 23);
    printf("%d\n", i);
}

/*********************************************************/

int[4] bb3 = [5,6,7,8];

int sum3(int[] xx = bb3 ...)
{
    int s;

    foreach (int x; xx)
        s += x;
    return s;
}

void test3()
{
    static int[3] a = [5, 8, 10];
    int i;

    i = sum3(11, 22, 34);
    assert(i == 67);
    i = sum3(a);
    assert(i == 23);
    i = sum3();
    assert(i == 26);
    printf("%d\n", i);
}


/*********************************************************/


void bug1993(int[][] y...)
{
}
void test5()
{
    bug1993(null);
    bug1993(null, null);
    bug1993([0], null);
    bug1993([0], [0]);
    bug1993(null, [0]);
}

/*********************************************************/

int main()
{
    test1();
    test2();
    test3();
    test5();

    printf("Success\n");
    return 0;
}
