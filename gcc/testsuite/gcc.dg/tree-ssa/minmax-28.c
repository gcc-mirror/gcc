/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2" } */

int max1(int a, int b)
{
    if (a <= b)
        return a < b ? b : a;
    return 0;
}

int max2(int a, int b)
{
    if (a <= b)
        return a > b ? a : b;
    return 0;
}

int max3(int a, int b)
{
    if (a < b)
        return a < b ? b : a;
    return 0;
}

int max4(int a, int b)
{
    if (a < b)
        return a > b ? a : b;
    return 0;
}

int max5(int a, int b)
{
    if (a <= b)
        return a <= b ? b : a;
    return 0;
}

int max6(int a, int b)
{
    if (a <= b)
        return a >= b ? a : b;
    return 0;
}

int max7(int a, int b)
{
    if (a < b)
        return a <= b ? b : a;
    return 0;
}

int max8(int a, int b)
{
    if (b > a)
        return a >= b ? a : b;
    return 0;
}

int max9(int a, int b)
{
    if (b >= a)
        return a < b ? b : a;
    return 0;
}

int max10(int a, int b)
{
    if (b >= a)
        return a > b ? a : b;
    return 0;
}

int max11(int a, int b)
{
    if (b > a)
        return a < b ? b : a;
    return 0;
}

int max12(int a, int b)
{
    if (b > a)
        return a > b ? a : b;
    return 0;
}

int max13(int a, int b)
{
    if (b >= a)
        return a <= b ? b : a;
    return 0;
}

int max14(int a, int b)
{
    if (b >= a)
        return a >= b ? a : b;
    return 0;
}

int max15(int a, int b)
{
    if (b > a)
        return a <= b ? b : a;
    return 0;
}

int max16(int a, int b)
{
    if (b > a)
        return a >= b ? a : b;
    return 0;
}

/* { dg-final { scan-tree-dump-not "MAX_EXPR" "dom2" } } */

