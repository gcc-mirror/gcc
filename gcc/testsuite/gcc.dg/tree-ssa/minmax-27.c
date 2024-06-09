/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2" } */


int min1(int a, int b)
{
    if (a <= b)
        return a < b ? a : b;
    return 0;
}

int min2(int a, int b)
{
    if (a <= b)
        return a > b ? b : a;
    return 0;
}

int min3(int a, int b)
{
    if (a < b)
        return a < b ? a : b;
    return 0;
}

int min4(int a, int b)
{
    if (a < b)
        return a > b ? b : a;
    return 0;
}

int min5(int a, int b)
{
    if (a <= b)
        return a <= b ? a : b;
    return 0;
}

int min6(int a, int b)
{
    if (a <= b)
        return a >= b ? b : a;
    return 0;
}

int min7(int a, int b)
{
    if (a < b)
        return a <= b ? a : b;
    return 0;
}

int min8(int a, int b)
{
    if (b > a)
        return a >= b ? b : a;
    return 0;
}

int min9(int a, int b)
{
    if (b >= a)
        return a < b ? a : b;
    return 0;
}

int min10(int a, int b)
{
    if (b >= a)
        return a > b ? b : a;
    return 0;
}

int min11(int a, int b)
{
    if (b > a)
        return a < b ? a : b;
    return 0;
}

int min12(int a, int b)
{
    if (b > a)
        return a > b ? b : a;
    return 0;
}

int min13(int a, int b)
{
    if (b >= a)
        return a <= b ? a : b;
    return 0;
}

int min14(int a, int b)
{
    if (b >= a)
        return a >= b ? b : a;
    return 0;
}

int min15(int a, int b)
{
    if (b > a)
        return a <= b ? a : b;
    return 0;
}

int min16(int a, int b)
{
    if (b > a)
        return a >= b ? b : a;
    return 0;
}

/* { dg-final { scan-tree-dump-not "MIN_EXPR" "dom2" } } */

