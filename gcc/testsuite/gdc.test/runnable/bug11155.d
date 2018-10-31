// PERMUTE_ARGS:

version(D_SIMD)
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
