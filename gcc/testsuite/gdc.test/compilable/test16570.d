static immutable int _a = 0;

enum Regression
{
    a = _a,
}

static assert(is(typeof(Regression.a) == immutable(Regression)));
