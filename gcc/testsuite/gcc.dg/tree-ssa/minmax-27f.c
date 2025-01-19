/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-evrp" } */


float min1(float a, float b)
{
    if (a <= b)
        return a < b ? a : b;
    return 0.0;
}

float min2(float a, float b)
{
    if (a <= b)
        return a > b ? b : a;
    return 0.0;
}

float min3(float a, float b)
{
    if (a < b)
        return a < b ? a : b;
    return 0.0;
}

float min4(float a, float b)
{
    if (a < b)
        return a > b ? b : a;
    return 0.0;
}

float min5(float a, float b)
{
    if (a <= b)
        return a <= b ? a : b;
    return 0.0;
}

float min6(float a, float b)
{
    if (a <= b)
        return a >= b ? b : a;
    return 0.0;
}

float min7(float a, float b)
{
    if (a < b)
        return a <= b ? a : b;
    return 0.0;
}

float min8(float a, float b)
{
    if (b > a)
        return a >= b ? b : a;
    return 0.0;
}

float min9(float a, float b)
{
    if (b >= a)
        return a < b ? a : b;
    return 0.0;
}

float min10(float a, float b)
{
    if (b >= a)
        return a > b ? b : a;
    return 0.0;
}

float min11(float a, float b)
{
    if (b > a)
        return a < b ? a : b;
    return 0.0;
}

float min12(float a, float b)
{
    if (b > a)
        return a > b ? b : a;
    return 0.0;
}

float min13(float a, float b)
{
    if (b >= a)
        return a <= b ? a : b;
    return 0.0;
}

float min14(float a, float b)
{
    if (b >= a)
        return a >= b ? b : a;
    return 0.0;
}

float min15(float a, float b)
{
    if (b > a)
        return a <= b ? a : b;
    return 0.0;
}

float min16(float a, float b)
{
    if (b > a)
        return a >= b ? b : a;
    return 0.0;
}

/* { dg-final { scan-tree-dump-not "MIN_EXPR" "evrp" } } */

