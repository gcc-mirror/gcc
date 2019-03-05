
version(D_SIMD)
{
    const __vector(float[4]) si = [1f, 1f, 1f, 1f];

    void main()
    {
        auto arr = si;
        return;
    }
}
