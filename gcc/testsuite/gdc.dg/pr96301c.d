// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96301
// { dg-additional-options "-fPIC" { target fpic } }
// { dg-do compile }
class Container
{
    int[] children;

    void remove(void* data)
    {
        int[] remove(int[] range)
        {
            auto result = range;
            if (result.front)
                return result;
            assert(0);
        }
        if (data)
            remove(children);
    }
}

int front(int[] a)
{
    return a.ptr[0];
}
