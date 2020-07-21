// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96254
// { dg-do compile }
struct map(alias fun)
{
    @property run()
    {
    }
}

struct Task(Args)
{
    Args _args;
}

template reduce(functions...)
{
    auto reduce(Args)(Args args)
    {
        alias RTask = Task!(typeof(args));
        auto task = RTask();
    }
}

void main() // { dg-error "'D main' is a nested function and cannot be accessed" }
{
    immutable delta = 1;
    reduce!"a + b"(map!({ immutable x = delta; })());
}
