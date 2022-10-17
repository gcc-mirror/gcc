// PERMUTE_ARGS:

static if (__traits(compiles, __vector(float[4])))
{
    alias float4 = __vector(float[4]);

    void foo(float4* ptr, float4 val)
    {
        assert((cast(ulong) &val & 0xf) == 0);
    }

    void main()
    {
        float4 v;
        foo(&v, v);
    }
}
else
    void main(){}
