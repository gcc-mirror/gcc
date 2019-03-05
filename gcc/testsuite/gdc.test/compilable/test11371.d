
version(D_SIMD)
{
    __vector(long[2]) f()
    {
        __vector(long[2]) q;
        return q;
    }

    enum __vector(long[2]) v = f();
}
