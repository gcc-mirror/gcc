// REQUIRED_ARGS: -inline -O -unittest

struct Cent
{
    ulong lo;
    ulong hi;
}

Cent add(Cent, Cent);

Cent sub(Cent c1, Cent c2)
{
    return add(c1, c2);
}

Cent udivmod(Cent c3, Cent c4)
{
    Cent quotient;
    Cent rem = sub(c3, c4);
    return quotient;
}
