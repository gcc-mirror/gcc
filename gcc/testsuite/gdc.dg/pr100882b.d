// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100882
// { dg-do compile }

auto iota(int, int)
{
    struct Result
    {
        this(int)
        {
        }
    }
    return Result();
}

auto iota(int end)
{
    int begin;
    return iota(begin, end);
}
