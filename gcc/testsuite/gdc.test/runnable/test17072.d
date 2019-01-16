/*
REQUIRED_ARGS: -inline
PERMUTE_ARGS: -release -O -dip25
*/

// https://issues.dlang.org/show_bug.cgi?id=17072

import core.thread;

void main()
{
        Thread.sleep(dur!"msecs"(0));
}
