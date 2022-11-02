// Issue 22784 - pragma(printf) applies to nested functions
// https://issues.dlang.org/show_bug.cgi?id=22784

import core.stdc.stdarg;
extern(C)
pragma(printf)
void fn(const(char)* fmt, ...)
{
    void inner(){}
}
