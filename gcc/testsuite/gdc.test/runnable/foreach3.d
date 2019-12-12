
import core.stdc.stdio;

struct Foo
{
    uint array[2];

    int opApply(int delegate(ref uint) dg)
    {
        int result;
        for (int i = 0; i < array.length; i++)
        {
            result = dg(array[i]);
            if (result)
                break;
        }
        return result;
    }
}


/**************************************************/

void test1()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (uint u; a)
    {
        i++;
        u++;
    }
    assert(i == 2);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

void test2()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (ref uint u; a)
    {
        i++;
        u++;
    }
    assert(i == 2);
    assert(a.array[0] == 74);
    assert(a.array[1] == 83);
}

/**************************************************/

void test3()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (ref uint u; a)
    {
        i++;
        if (i)
            break;
        u++;
    }
    assert(i == 1);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

void test4()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (ref uint u; a)
    {
        i++;
        if (i == 1)
            continue;
        u++;
    }
    assert(i == 2);
    assert(a.array[0] == 73 && a.array[1] == 83);
}

/**************************************************/

void test5()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

Loop:
    while (1)
    {
        foreach (ref uint u; a)
        {
            i++;
            if (i)
                break Loop;
            u++;
        }
    }
    assert(i == 1);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

void test6()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

Loop:
    while (1)
    {
        foreach (ref uint u; a)
        {
            i++;
            if (i == 1)
                continue Loop;
            u++;
        }
        break;
    }
    assert(i == 3);
    assert(a.array[0] == 74);
    assert(a.array[1] == 83);
}

/**************************************************/

void test7()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (ref uint u; a)
    {
        i++;
        if (i)
            goto Label;
        u++;
    }
    assert(0);
Label:
    assert(i == 1);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

void test8_x(Foo a)
{
    int i;
    foreach (ref uint u; a)
    {
        i++;
        if (i)
            return;
        u++;
    }
}

void test8()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    test8_x(a);
    assert(i == 0);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

int test9_x(Foo a)
{
    int i;
    foreach (ref uint u; a)
    {
        i++;
        if (i)
            return 67;
        u++;
    }
    return 23;
}

void test9()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    i = test9_x(a);
    assert(i == 67);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

int test10_x(Foo a)
{
    int i;
    foreach (ref uint u; a)
    {
        i++;
        if (i)
            return i;
        u++;
    }
    return 23;
}

void test10()
{
    Foo a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    i = test10_x(a);
    assert(i == 1);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
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
    test10();

    printf("Success\n");
    return 0;
}
