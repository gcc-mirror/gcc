// https://issues.dlang.org/show_bug.cgi?id=18645

immutable INIT = 42;

enum A
{
    x = INIT,
    y
}
