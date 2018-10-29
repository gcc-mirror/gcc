// REQUIRED_ARGS: -w

version(unittest) template symsToStrs(fields...)
{
    static if (fields.length == 0)
        enum symsToStrs = ["hello"];
    else
        enum symsToStrs = ["world"];
}
