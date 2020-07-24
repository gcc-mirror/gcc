// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96301
// { dg-additional-options "-fPIC" { target fpic } }
// { dg-do compile }
struct Type
{
    size_t length;
    int* ptr;
}

class Container
{
    Type children;

    void remove(void* data)
    {
        Type remove(Type range)
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

int front(Type a)
{
    return a.ptr[0];
}
