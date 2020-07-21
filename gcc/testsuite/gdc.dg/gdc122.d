// https://bugzilla.gdcproject.org/show_bug.cgi?id=122
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

class TaskPool
{
    template reduce(functions...)
    {
        auto reduce(Args)(Args args)
        {
            alias RTask = Task!(typeof(args));
            auto task = RTask();
        }
    }
}

TaskPool taskPool() {
    return new TaskPool;
}

void test122()
{
    enum delta = 1;
    taskPool.reduce!"a + b"(map!({ immutable x = delta; })());
}
