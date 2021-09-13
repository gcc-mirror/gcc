// https://issues.dlang.org/show_bug.cgi?id=2920
// recursive templates blow compiler stack
// template_17_A.
/*
TEST_OUTPUT:
---
fail_compilation/fail280.d(13): Error: template instance `fail280.t!500` recursive expansion exceeded allowed nesting limit
---
*/

template t(int i)
{
    const int x = t!(i + 1).x;
}

void main()
{
    int i = t!(0).x;
}
