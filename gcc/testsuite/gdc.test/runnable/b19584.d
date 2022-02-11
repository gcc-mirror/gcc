/*
REQUIRED_ARGS: -O
*/
void main()
{
    int a = 711;
    assert(182215 == (a | (a << 8)));
    assert(182727 == (a * (1 + (1 << 8))));

    int b = 31;
    assert(511 == (b | (b << 4)));
    assert(527 == (b * (1 + (1 << 4))));
}
