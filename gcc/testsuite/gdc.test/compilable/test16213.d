// https://issues.dlang.org/show_bug.cgi?id=16213

enum Id(size_t i) = i;
void main()
{
    int[5] y;
    y[ Id!($) - 1 ] = 3;
}
