// REQUIRED_ARGS: -main
// LINK:
module test21299d;

struct DefaultPredicates
{
    struct IsEqual(T)
    {
        static opCall(in T, in T)
        {
            return 0;
        }
    }
}

void moveToEnd(T, Pred = DefaultPredicates.IsEqual!T)(T[] array, T element, Pred pred = Pred.init)
{
    pred(array[0], element);
}

class Task
{
    void removeTerminationHook(void delegate() hook)
    {
        moveToEnd([], hook);
    }
}
