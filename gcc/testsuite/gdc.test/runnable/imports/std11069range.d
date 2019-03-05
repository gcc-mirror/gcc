module imports.std11069range;

public import imports.std11069array;

template isInputRange(R)
{
    enum bool isInputRange = is(typeof(
    (inout int = 0)
    {
        R r = void;       // can define a range object
        if (r.empty) {}   // can test for empty
        r.popFront();     // can invoke popFront()
        auto h = r.front; // can get the front of the range
    }));
}
