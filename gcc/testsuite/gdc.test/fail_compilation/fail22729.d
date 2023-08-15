// https://issues.dlang.org/show_bug.cgi?id=22729

/*
TEST_OUTPUT:
---
fail_compilation/fail22729.d(12): Error: field `getChildAtPosition` not allowed in interface
---
*/

interface ContainerFunctionSetI
{
    Tuple!(WidgetI) getChildAtPosition;
}

interface WidgetI : ContainerFunctionSetI
{
}

class Form : WidgetI
{
}

template Tuple(Specs)
{
    auto areCompatibleTuples(Tup2)(Tuple tup1, Tup2 tup2)
    {
        tup1.field == tup2;
    }

    struct Tuple
    {
        Specs field;

        bool opEquals(R)(R) if (areCompatibleTuples!R)
        {
        }

    }
}
