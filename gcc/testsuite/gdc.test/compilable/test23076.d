/* REQUIRED_ARGS: -O -inline
 */

// https://issues.dlang.org/show_bug.cgi?id=23076

struct S
{
    int depthLow = 0;
    int depthHigh = 30000;

    void fun(int* pixels)
    {
        float b = depthLow;
        int depthA = cast(int)(b);
        int depthB = cast(short)(cast(float)depthHigh * cast(float)depthLow);
        pixels[depthA] = depthB;
    }
}

/**********************/

float mul3(float a, float b, float t)
{
    return t * b * a;
}

class A
{
    ushort depthLow = 0;
    ushort depthHigh = 30000;

    void fun(short* pixels)
    {
        short depthA = (cast(short)(mul3(depthHigh, depthLow, 0)));
        short depthB = (cast(short)(mul3(depthLow, depthHigh, 0)));
        pixels[depthA] = depthB;
    }
}
