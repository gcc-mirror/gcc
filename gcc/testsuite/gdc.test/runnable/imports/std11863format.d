module imports.std11863format;

import imports.std11863bitmanip;
import imports.std11863conv;

struct FormatSpec(Char)
{
    string toString()
    {
        // text(width)
        return to!string(1);    // instantiate toImpl!(string, int)
    }
}
