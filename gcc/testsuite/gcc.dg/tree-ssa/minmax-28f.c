/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-evrp" } */

float max1(float a, float b)
{
    if (a <= b)
        return a < b ? b : a;
    return 0.0;
}

float max2(float a, float b)
{
    if (a <= b)
        return a > b ? a : b;
    return 0.0;
}

float max3(float a, float b)
{
    if (a < b)
        return a < b ? b : a;
    return 0.0;
}

float max4(float a, float b)
{
    if (a < b)
        return a > b ? a : b;
    return 0.0;
}

float max5(float a, float b)
{
    if (a <= b)
        return a <= b ? b : a;
    return 0.0;
}

float max6(float a, float b)
{
    if (a <= b)
        return a >= b ? a : b;
    return 0.0;
}

float max7(float a, float b)
{
    if (a < b)
        return a <= b ? b : a;
    return 0.0;
}

float max8(float a, float b)
{
    if (b > a)
        return a >= b ? a : b;
    return 0.0;
}

float max9(float a, float b)
{
    if (b >= a)
        return a < b ? b : a;
    return 0.0;
}

float max10(float a, float b)
{
    if (b >= a)
        return a > b ? a : b;
    return 0.0;
}

float max11(float a, float b)
{
    if (b > a)
        return a < b ? b : a;
    return 0.0;
}

float max12(float a, float b)
{
    if (b > a)
        return a > b ? a : b;
    return 0.0;
}

float max13(float a, float b)
{
    if (b >= a)
        return a <= b ? b : a;
    return 0.0;
}

float max14(float a, float b)
{
    if (b >= a)
        return a >= b ? a : b;
    return 0.0;
}

float max15(float a, float b)
{
    if (b > a)
        return a <= b ? b : a;
    return 0.0;
}

float max16(float a, float b)
{
    if (b > a)
        return a >= b ? a : b;
    return 0.0;
}

/* { dg-final { scan-tree-dump-not "MAX_EXPR" "evrp" } } */

