// https://bugzilla.gdcproject.org/show_bug.cgi?id=31
// { dg-do compile }

class RedBlackTree(T, alias less)
{
    struct Range
    {
        @property empty() { }
    }

    Range opSlice()
    {
        return Range();
    }
}

auto redBlackTree(alias less, E)()
{
    return new RedBlackTree!(E, less);
}

void test31()
{
    redBlackTree!((a){}, double)();
}
