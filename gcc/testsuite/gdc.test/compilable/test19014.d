// https://issues.dlang.org/show_bug.cgi?id=19014

import core.stdc.config;

void main()
{
    if (true)
    {
    	static import core.stdc.math;
    }
    static assert(!__traits(compiles, core.stdc.math.cos(0)));
}
