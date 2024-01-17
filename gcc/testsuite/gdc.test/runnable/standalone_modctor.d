// REQUIRED_ARGS: -Irunnable/imports
// EXTRA_SOURCES: imports/standalone_b.d
// PERMUTE_ARGS: -cov

import standalone_b;
import core.attribute : standalone;

immutable int* x;

@standalone @system shared static this()
{
    x = new int(1);
}

void main()
{
    assert(*x == 1);
    assert(*y == 2);
}
