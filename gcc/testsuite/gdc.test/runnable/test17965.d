/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=17965

import core.stdc.math;

struct Point{double x,y;}

Point foo10()
{
    Point result = Point(1.0, 2.0);
    return result;
}

Point foo20()
{
    Point result;
    return result;
}

void main()
{
    auto p = foo10();
    assert(p.x == 1.0);
    assert(p.y == 2.0);

    auto q = foo20();
    assert(isnan(q.x));
    assert(isnan(q.y));
}
