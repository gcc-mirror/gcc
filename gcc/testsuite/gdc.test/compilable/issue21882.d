// REQUIRED_ARGS: -preview=dip1021
// https://issues.dlang.org/show_bug.cgi?id=21882
bool buildPath()
{
    struct ByCodeUnitImpl
    {
        auto opIndex(size_t) { }
    }
    return isRandomAccessRange!ByCodeUnitImpl;
}

enum isRandomAccessRange(R) = is(typeof(lvalueOf!R[1]));

ref T lvalueOf(T)();
