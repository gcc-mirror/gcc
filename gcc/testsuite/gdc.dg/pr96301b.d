// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96301
// { dg-additional-options "-fPIC" { target fpic } }
// { dg-do compile }
class Container
{
    int[100] children;

    void remove(void* data)
    {
        int[100] remove(int[100] range)
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

int front(int[100] a)
{
    return a.ptr[0];
}
