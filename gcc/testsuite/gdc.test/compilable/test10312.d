
static if (__traits(compiles, __vector(float[4])))
{
    const __vector(float[4]) si = [1f, 1f, 1f, 1f];

    void main()
    {
        auto arr = si;
        return;
    }
}
