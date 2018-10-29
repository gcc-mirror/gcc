debug(1) import std.stdio;
const int anything = -1000; // Line #2
dchar[] SomeFunc( dchar[] pText, out int pStopPosn)
{
    if (pText.length == 0)
        pStopPosn = 0;
    else
        pStopPosn = -1;
    debug(1) writefln("DEBUG: using '%s' we get %d", pText, pStopPosn);
    return pText.dup;
}

int main(char[][] pArgs)
{
    int sp;

    SomeFunc("123", sp);
    debug(1) writefln("DEBUG: got %d", sp);
    assert(sp == -1);

    SomeFunc("", sp);
//    if (sp != 0){} // Line #22
    debug(1) writefln("DEBUG: got %d", sp);
    assert(sp == -1);
    return 0;
}

