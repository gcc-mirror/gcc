module imports.test11563std_traits;

import imports.test11563std_range;

bool startsWith(R1, R2)(R1 doesThisStart, R2 withThis)
if (isInputRange!R1)
{
    return true;
}

template moduleName(alias T)
{
    static if (T.stringof.startsWith("module "))
    {
        enum moduleName = "b";
    }
    else
    {
        pragma(msg, "--error--");
    }
}

