// REQUIRED_ARGS: -O -inline

struct Cent
{
    ulong lo;  // low 64 bits
    ulong hi;  // high 64 bits
}

pure bool tst(Cent c)
{
    return c.hi || c.lo;
}

pure Cent dec(Cent c);
pure Cent shl(Cent c, uint n);

pure Cent udivmod(Cent c1, Cent c2, out Cent modulus)
{
    ulong v1 = shl(c2, 3).hi;

    Cent quotient;

    if (tst(quotient))
        quotient = dec(quotient);

    return quotient;
}
