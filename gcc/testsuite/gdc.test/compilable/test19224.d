// https://issues.dlang.org/show_bug.cgi?id=19224
static if (__traits(compiles, __vector(float[4])))
{
    float sum(const float[4] val)
    {
        float sum = 0;
        foreach (x; val) sum += x;
        return sum;
    }

    alias float4 = __vector(float[4]);

    enum x = sum(float4.init.array);
    static assert(x is float.nan);

    enum y = sum(float4(1).array);
    static assert(y == 4);
}
