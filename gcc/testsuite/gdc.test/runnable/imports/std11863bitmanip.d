module imports.std11863bitmanip;

import imports.std11863format : FormatSpec;

struct BitArray
{
    void toString(scope void delegate(const(char)[]) sink,
                  FormatSpec!char fmt) const
    {
    }
}
