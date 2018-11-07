
import core.stdc.stdio;

/**************************************************/

void test1()
{
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

    foreach (uint u; a)
    {
        i++;
        u++;
    }
    assert(i == 2);
    assert(a["hello"] == 73);
    assert(a["world"] == 82);
}

/**************************************************/

void test2()
{
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

    foreach (ref uint u; a)
    {
        i++;
        u++;
    }
    assert(i == 2);
    assert(a["hello"] == 74);
    assert(a["world"] == 83);
}

/**************************************************/

void test3()
{
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

    foreach (ref uint u; a)
    {
        i++;
        if (i)
            break;
        u++;
    }
    assert(i == 1);
    assert(a["hello"] == 73);
    assert(a["world"] == 82);
}

/**************************************************/

void test4()
{
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

    foreach (ref uint u; a)
    {
        i++;
        if (i == 1)
            continue;
        u++;
    }
    assert(i == 2);
    assert((a["hello"] == 73 && a["world"] == 83) ||
           (a["hello"] == 74 && a["world"] == 82));
}

/**************************************************/

void test5()
{
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

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
    assert(a["hello"] == 73);
    assert(a["world"] == 82);
}

/**************************************************/

void test6()
{
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

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
    assert(a["hello"] == 74);
    assert(a["world"] == 83);
}

/**************************************************/

void test7()
{
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

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
    assert(a["hello"] == 73);
    assert(a["world"] == 82);
}

/**************************************************/

void test8_x(uint[char[]] a)
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
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

    test8_x(a);
    assert(i == 0);
    assert(a["hello"] == 73);
    assert(a["world"] == 82);
}

/**************************************************/

int test9_x(uint[char[]] a)
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
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

    i = test9_x(a);
    assert(i == 67);
    assert(a["hello"] == 73);
    assert(a["world"] == 82);
}

/**************************************************/

int test10_x(uint[char[]] a)
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
    uint[char[]] a;
    int i;

    a["hello"] = 73;
    a["world"] = 82;

    i = test10_x(a);
    assert(i == 1);
    assert(a["hello"] == 73);
    assert(a["world"] == 82);
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
