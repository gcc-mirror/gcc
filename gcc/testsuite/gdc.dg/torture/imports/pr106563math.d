module imports.pr106563math;

T nextPow2(T)(const T val)
{
    return powIntegralImpl(val);
}

pragma(inline, true)
T powIntegralImpl(T)(T)
{
    return 1;
}
