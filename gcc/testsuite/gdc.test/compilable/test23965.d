// https://issues.dlang.org/show_bug.cgi?id=23965
// REQUIRED_ARGS: -de
deprecated:

struct S {}

void fun()
{
    S[] arr;
    arr ~= S();
}
