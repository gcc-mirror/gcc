module imports.test11563std_range;

public import imports.test11563std_array;

template isInputRange(R)
{
    enum bool isInputRange = is(typeof(
    {
        R r = void;
        r.popFront();
    }));
}
