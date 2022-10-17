/*
REQUIRED_ARGS: -de
*/

// This test can be removed once the deprecation period is over
deprecated void appendSlices ( Types ... ) ( ref void[][] slices, ref Types x )
{
    foreach (i, T; Types)
    {
        static if (is(T Element: Element[]))
        {
            static if (is(T == Element[]))
            {
                slices ~= (cast(void*)(&x[i]))[0 .. size_t.sizeof];
            }
            // Append a slice to the array content.
            slices ~= x[i];
        }
        else
        {
            slices ~= (cast(void*)(&x[i]))[0 .. x[i].sizeof];
        }
    }
}

deprecated void myTest()
{
    void[][] slices;
    char[] str = "Hello World!".dup;
    appendSlices(slices, str);
}
