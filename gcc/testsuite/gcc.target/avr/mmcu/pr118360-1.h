int fun9_16 (int a, int b)
{
    if (a & 1)
        b = b | 0x9;
    return b;
}

long fun9_32 (int a, long b)
{
    if (a & 1)
        b = b | 0x9;
    return b;
}

int fun8_16 (int a, int b)
{
    if (a & 1)
        b = b | 0x8;
    return b;
}

long fun8_32 (int a, long b)
{
    if (a & 1)
        b = b | 0x8;
    return b;
}
